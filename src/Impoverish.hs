module Impoverish where
import qualified EnrichedLambda as EL
import EnrichedLambda (Def)
import qualified Lambda as L
import Constant
import Pattern
import Symbol
import TypeDef
import Data.Maybe (fromJust)
import Data.Set (toList)

-- Transform the enriched lambda calculus to ordinary lambda calculus.
impoverish :: EL.Expr TypeDef -> L.Expr
impoverish enriched = case EL.out enriched of
    -- An variable in ELC can be literally translated to a variable in LC.
    (EL.VarExpr v) -> L.Var v

    -- A constant in ELC can be literally translated to a constant in LC.
    (EL.ConstExpr k) -> L.Const k

    -- An application in ELC can be translated by recursively applying the
    -- impoverish algorithm to the operands.
    (EL.AppExpr e f) -> L.App (impoverish e) (impoverish f)

    -- ELC abstractions without pattern matching can be directly translated
    -- into LC abstractions by recursively impoverishing the body.
    -- The translation of pattern-matching ELC abstractions depends
    -- on the pattern. See section 6.1 "Transforming Pattern-matching Lambda
    -- Abstractions" (page 104).
    -- 1. If k is a constant pattern
    --    \k.E <=> \v.IF (= k v) E FAIL
    -- 2. If p is a product-constructor pattern
    --    \(t p1 ... pr).E <=> UNPACK-PRODUCT-t (\p1 ... \pr.E)
    -- 3. If p is a sum-constructor pattern
    --    \(s p1 ... pr).E <=> UNPACK-SUM-s (\p1 ... \pr.E)
    (EL.AbstrExpr p e) -> case p of
        (VarPat x) -> L.Abstr x (impoverish e)

        -- Constant pattern.
        (ConstPat k)
            -> let e' = impoverish e
                   v  = head $ L.nonfreeSymbols e'
                   -- b is (IF (= k v) E FAIL)
                   b  = L.makeApp (L.Const IfConst)
                                  (L.App (L.App (L.Const EqConst)
                                                (L.Const k))
                                         (L.Var v))
                                  [ e'
                                  , L.Const FailConst
                                  ]
                        in L.Abstr v b

        -- Product or sum constructors.
        (ConstrPat tdef t ps)
            -> let arity = fromJust $ getConstructorArity tdef t
                   tag   = fromJust $ getStructureTag tdef t
               in case productOrSumType tdef of
                 -- Product-constructor pattern.
                    ProductType -> L.makeApp (L.Const UnpackProductConst)
                                             (L.Const (IntConst arity))
                                             [impoverish $ EL.makeAbstr ps e]

                 -- Sum-constructor pattern.
                    SumType     -> L.makeApp (L.Const UnpackSumConst)
                                             (L.Const (IntConst tag))
                                             [ L.Const (IntConst arity)
                                             , impoverish (EL.makeAbstr ps e)]
                                
    -- Transform let expression.
    (EL.LetExpr (p, b) e) -> case p of
         -- Simple let expression.
         -- let v = B in E <=> (\v.E) B
        (VarPat v) -> L.App (L.Abstr v (impoverish e)) (impoverish b)

        -- Irrefutable and refutable non-simple lets.
        (ConstrPat tdef t ps)
            -> if irrefutable p
               then let v = L.getNewVariable (impoverish e)
                        arity = case getConstructorArity tdef t of
                                  Nothing  -> error (show (t, tdef))
                                  (Just n) -> n
                        makeSel i = EL.appExpr (EL.constExpr SelectConst)
                                               (EL.constExpr (IntConst i))
                        r = length ps
                        sels = map makeSel [1..r]
                        apps = map (`EL.appExpr` EL.varExpr v) sels
                    in impoverish $ EL.letExpr (VarPat v, b)
                                  $ EL.makeLet (zip ps apps) e


               --impoverishIrrefutableProductLet tdef s ps b e
               -- TODO 6.2.7 p115.
               else let newDef = conformalityTransformation (p, b)
                    in impoverish (EL.letExpr newDef e)

    -- Transform a letrec expression.
    -- TODO: handle refutable letrec into irrefutable letrec.
    (EL.LetrecExpr defs e) -> f
        where f
                -- Transform irrefutable letrec into irrefutable let,
                -- then recursively apply impoverish on the result. (p114)
                | all (irrefutable . fst) defs
                    = let nTupType   = tupleType (length defs)
                          nTupConstr = head $ getTypeConstructors nTupType
                          nTupConstrName = getConstrName nTupConstr
                          p = ConstrPat nTupType nTupConstrName (map fst defs)
                          b = EL.makeApp
                              (EL.constExpr $ ConstrConst (DataTag 0) (length defs))
                              (snd $ head defs)
                              (map snd $ tail defs)
                          abstr = EL.abstrExpr p b
                          [f, x] = EL.getNewVariables abstr 2
                          yComb = let [fPat, xPat] = map VarPat [f, x]
                                      [fVar, xVar] = map EL.varExpr [f, x]
                                      body = EL.abstrExpr xPat
                                             $ EL.appExpr fVar
                                             $ EL.appExpr xVar xVar
                                  in EL.abstrExpr fPat (EL.appExpr body body)
                          yExpr = EL.appExpr yComb abstr
                      in impoverish $ EL.letExpr (p,yExpr) e

                -- Transform general letrec into irrefutable letrec.
                -- TODO 6.2.7 p115.
                | otherwise = let newDefs = map
                                            (\def -> if irrefutable (fst def)
                                                     then def
                                                     else conformalityTransformation def)
                                            defs
                            in impoverish (EL.letrecExpr newDefs e)

    -- Transform a case expression.
    (EL.CaseExpr expr cs) -> case cs of
        -- Case expressions involving a product type (p122 6.3.1).
        -- possible optimization p124 6.3.3 
        [(ConstrPat (TypeDef _ [constr]) sym vs, e1)]
            -> let arity = length $ getConstrFields constr
                   unpack = EL.appExpr (EL.constExpr UnpackProductConst)
                                       (EL.constExpr (IntConst arity)) -- XXX
                   abstr = EL.makeAbstr vs e1
               in impoverish $ EL.makeApp unpack abstr [expr]


        -- Case expressions involving a sum type (p122 6.3.2).
        -- possible optimization p124 6.3.3 
        -- NOTE: using alternative implementation with [] and FAIL.
        -- XXX
        otherwise -> let (ConstrPat tdef _ _) = fst $ head cs
                         n = length (getTypeConstructors tdef)
                         tag sym = fromJust (getStructureTag tdef sym)
                         arity sym = fromJust (getConstructorArity tdef sym)
                         unpack (ConstrPat _ si vs, e)
                             = EL.makeApp
                               (EL.constExpr UnpackSumConst)
                               (EL.constExpr (IntConst (tag si)))
                               [ EL.constExpr (IntConst (arity si))
                               , EL.makeAbstr vs e
                               ]
                         unpackTerms = map unpack cs
                         abstrParam = EL.getNewVariable
                                      $ foldl1 EL.appExpr unpackTerms
                         abstrBody = foldl1
                                     EL.fatbarExpr
                                     $ map (`EL.appExpr` EL.varExpr abstrParam)
                                           unpackTerms
                     in impoverish
                        $ EL.appExpr (EL.abstrExpr (VarPat abstrParam) abstrBody) expr

                   
    (EL.FatbarExpr e f) ->
        L.makeApp (L.Const FatbarConst) (impoverish e) [impoverish f]

-- Transform a refutable pattern into an irrefutable one (p117).
conformalityTransformation :: Def TypeDef -> Def TypeDef
conformalityTransformation (pat@(ConstrPat tdef sym pats), b) = (rhs, lhs)
    where
      rhs = ConstrPat nTupType (getConstrName nTupConstr) (map VarPat vars)
      lhs = EL.fatbarExpr app (EL.constExpr ErrorConst)
      app = EL.appExpr abstr b
      abstr = EL.abstrExpr pat $ EL.makeApp (EL.constExpr $ ConstrConst (DataTag 0) n)
                                            (EL.varExpr $ head vars)
                                            (map EL.varExpr $ tail vars)
      n = length (getConstrFields nTupConstr)
      nTupConstr = head $ getTypeConstructors nTupType
      nTupType = tupleType (length vars)
      vars = toList (patVariables pat)

-- A pattern is irrefutable if it is
-- 1. a variable, or
-- 2. a product pattern of the form (t p1 ... pr) where p1, ..., pr
--    are irrefutable patterns.
irrefutable :: Pattern TypeDef -> Bool
irrefutable (VarPat _)                       = True
irrefutable (ConstrPat (TypeDef _ [_]) _ ps) = all irrefutable ps
irrefutable _                                = False

-- Literally lift a simple lambda expression into the enriched lambda calculus.
embed :: L.Expr -> EL.Expr t
embed (L.Var s     ) = EL.varExpr s
embed (L.Const c   ) = EL.constExpr c
embed (L.App e f   ) = EL.appExpr (embed e) (embed f)
embed (L.Abstr s e ) = EL.abstrExpr (VarPat s) (embed e)
