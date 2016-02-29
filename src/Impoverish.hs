module Impoverish where
import qualified EnrichedLambda as EL
import EnrichedLambda (Pattern(..), ConstrType(..), Def)
import qualified Lambda as L
import Constant

-- Transform the enriched lambda calculus to ordinary lambda calculus.
impoverish :: EL.Expr -> L.Expr

-- An variable in ELC can be literally translated to a variable in LC.
impoverish (EL.VarExpr v)     = L.Var v

-- An application in ELC can be translated by recursively applying the
-- impoverish algorithm to the operands.
impoverish (EL.AppExpr e f)   = L.App (impoverish e) (impoverish f)

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
impoverish (EL.AbstrExpr p e)
    = case p
      -- Constant pattern.
      of (ConstPat k)
             -> let e' = impoverish e
                    v  = head $ L.nonfreeSymbols e'
                    -- b is (IF (= k v) E FAIL)
                    b  = L.makeApp (L.Var "IF")
                                   (L.App (L.App (L.Var "=")
                                                 (L.Const k))
                                          (L.Var v))
                                   [ e'
                                   , L.Var "FAIL"
                                   ]
                         in L.Abstr v b

         -- Product-constructor pattern.
         (ConstrPat EL.ProductConstr t ps)
             -> L.App (L.Var $ "UNPACK-PRODUCT-" ++ t)
                      (impoverish $ EL.makeAbstr ps e)

         -- Sum-constructor pattern.
         (ConstrPat EL.SumConstr s ps)
             -> L.App (L.Var $ "UNPACK-SUM-" ++ s)
                      (impoverish $ EL.makeAbstr ps e)

                                
-- Transform let expression.
impoverish (EL.LetExpr (p, b) e)
    = case p
      -- Simple let expression.
      -- let v = B in E <=> (\v.E) B
      of (VarPat v) -> L.App (L.Abstr v (impoverish e)) (impoverish b)
         (ConstrPat t s ps)
             -> if irrefutable p
                then impoverishIrrefutableProductLet s ps b e
                else undefined --impoverishRefutableProductLet

-- Transform a letrec expression.
-- TODO: handle refutable letrec into irrefutable letrec.
impoverish (EL.LetrecExpr ds e)
    -- Transform irrefutable letrec into irrefutable let, then recursively apply
    -- impoverish on the result. (p114)
    | all (irrefutable . fst) ds
        = let n = length ds
              tup = "TUP-" ++ show n
              p = ConstrPat ProductConstr tup (map fst ds)
              b = EL.makeApp (EL.VarExpr tup) (snd$head ds) (map snd ds)
              yExpr = EL.AppExpr (EL.VarExpr "Y") (EL.AbstrExpr p b)
          in impoverish $ EL.LetExpr (p,yExpr) e

    -- Transform general letrec into irrefutable letrec.
    | otherwise = undefined

-- Transform a case expression.
-- This case is not total. It operates under the assumption that, if
-- the list of cases contains more than one entry, all patterns must be
-- sum patterns. Type checking should make sure that this is always
-- the case.
-- TODO: CASE-T should be replaced by CASE-{type of sum pattern}.
impoverish (EL.CaseExpr v cs)
    = case cs of

        -- Case expressions involving a product type (p122 6.3.1).
        [(ConstrPat ProductConstr t vs, e)]
            -> let unpack = EL.VarExpr ("UNPACK-PRODUCT-" ++ t)
                   abstr  = EL.makeAbstr vs e
               in impoverish $ EL.makeApp unpack abstr [EL.VarExpr v]

        -- Case expressions involving a sum type (p122 6.3.2).
        otherwise
            -> let caseTerm = EL.VarExpr "CASE-T"
                   vTerm = EL.VarExpr v
                   unpack (ConstrPat SumConstr si vs, e)
                       = EL.AppExpr (EL.VarExpr$"UNPACK-SUM-"++si) (EL.makeAbstr vs e)
                   unpackTerms = map ((`EL.AppExpr` vTerm) . unpack) cs
               in impoverish $ EL.makeApp caseTerm vTerm unpackTerms
                   
impoverish (EL.FatbarExpr e f)
    = L.makeApp (L.Var "FATBAR") (impoverish e) [impoverish f]

-- Transforms an irrefutable let, first into a simple let, then to ordinary
-- lambda calculus.
-- This function is not total.
-- Its behaviour is only defined if it has already been proven that ps
-- is a list of irrefutable patterns.
impoverishIrrefutableProductLet :: EL.Symbol -> [Pattern] -> EL.Expr -> EL.Expr -> L.Expr
impoverishIrrefutableProductLet s ps b e =
    impoverish $ EL.LetExpr (VarPat v, b) $ EL.makeLet (zip ps selections) e
    where          
      v = L.getNewVariable (impoverish e)
      r = length ps
      selections = map (EL.VarExpr . (("SEL-" ++ s) ++) . show) [1..r]



-- A pattern is irrefutable if it is
-- 1. a variable, or
-- 2. a product pattern of the form (t p1 ... pr) where p1, ..., pr
--    are irrefutable patterns.
irrefutable :: Pattern -> Bool
irrefutable (VarPat _)                     = True
irrefutable (ConstrPat SumConstr _ _)      = False
irrefutable (ConstrPat ProductConstr _ ps) = all irrefutable ps

-- Literally lift a simple lambda expression into the enriched lambda calculus.
embed :: L.Expr -> EL.Expr
embed (L.Var s     ) = EL.VarExpr s
embed (L.Const c   ) = EL.ConstExpr c
embed (L.App e f   ) = EL.AppExpr (embed e) (embed f)
embed (L.Abstr s e ) = EL.AbstrExpr (VarPat s) (embed e)
