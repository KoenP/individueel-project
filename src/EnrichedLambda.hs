{-# LANGUAGE DeriveFunctor #-}
module EnrichedLambda where
import Symbol
import Pattern
import Constant
import TypeDef
import Fix
import Control.Monad (join)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (fix)

-- An expression in the enriched lambda calculus.
data ExprF typeinfo e = VarExpr    Symbol
                      | ConstExpr  Constant
                      | AppExpr    e                   e
                      | AbstrExpr  (Pattern typeinfo)  e
                      | LetExpr    (DefF typeinfo e)   e
                      | LetrecExpr [DefF typeinfo e]   e
                      | FatbarExpr e                   e
                      | CaseExpr   e                   [(Pattern typeinfo, e)]
                        deriving Functor
instance (Show typeinfo, Show e) => Show (ExprF typeinfo e) where
    show (VarExpr x) = "VarExpr " ++ x
    show (ConstExpr c) = "ConstExpr " ++ show c
    show (AppExpr e f) = "AppExpr " ++ show e ++ " " ++ show f

type DefF typeinfo e = (Pattern typeinfo, e) -- Definition
type Def typeinfo = DefF typeinfo (Expr typeinfo)
type AnnExpr typeinfo ann = Fix (Ann ann (ExprF typeinfo))
type Expr typeinfo = Fix (ExprF typeinfo) -- Normal ELC expression

-- Pseudoconstructor boilerplate.
varExpr           = In . VarExpr
constExpr         = In . ConstExpr
appExpr e f       = In (AppExpr e f)
abstrExpr x f     = In (AbstrExpr x f)
letExpr def e     = In (LetExpr def e)
letrecExpr defs e = In (LetrecExpr defs e)
fatbarExpr e f    = In (FatbarExpr e f)
caseExpr e cs     = In (CaseExpr e cs)

-- Given an ELC expression with no type information, return the expression
-- with type information.
fillInTypeInfo :: (Symbol -> Maybe TypeDef)
               -> Expr ()
               -> Either ConstructorUndefinedError (Expr TypeDef)
fillInTypeInfo g e =
    fmap In $ case out e of
      (VarExpr x)         -> pure (maybe (VarExpr x) ConstExpr (getConstrConstant g x))
      (ConstExpr k)       -> pure (ConstExpr k)
      (AppExpr e f)       -> AppExpr <$> fillInTypeInfo g e <*> fillInTypeInfo g f
      (AbstrExpr pat e)   -> AbstrExpr <$> fillInPatTypeInfo g pat <*> h e
      (LetExpr def e)     -> LetExpr <$> fillInDefTypeInfo def <*> h e
      (LetrecExpr defs e) -> LetrecExpr <$> sequence (map fillInDefTypeInfo defs) <*> h e
      (FatbarExpr e f)    -> FatbarExpr <$> h e <*> h f
      (CaseExpr s cases)
          -> CaseExpr
             <$> fillInTypeInfo g s
             <*> sequence
                 (map (\(p,e)->(,) <$> fillInPatTypeInfo g p <*> h e) cases)
    where
      h = fillInTypeInfo g
      fillInDefTypeInfo (p,e) = (,) <$> fillInPatTypeInfo g p <*> h e

getConstrConstant :: (Symbol -> Maybe TypeDef) -> Symbol -> Maybe Constant
getConstrConstant f s = join $ getFromTypeDef <$> f s
    where
      getFromTypeDef :: TypeDef -> Maybe Constant
      getFromTypeDef (TypeDef _ constrs) = do
        (index, constr) <- find ((==s) . getConstrName . snd) (zip [0..] constrs)
        return $ ConstrConst (DataTag index) (length $ getConstrFields constr)

-- Convenience function to transform a list of patterns and an expression
-- into a single nested abstraction.
makeAbstr :: [Pattern typeinfo] -> (Expr typeinfo) -> (Expr typeinfo)
makeAbstr ps e = foldr ((.) . abstrExpr) id ps e

-- Convenience function for when you want to make a set of nested applications
-- from a list of expressions containing at least two expressions.
makeApp :: (Expr typeinfo) -> (Expr typeinfo) -> [(Expr typeinfo)] -> (Expr typeinfo)
makeApp e1 e2 es = foldl appExpr (appExpr e1 e2) es

-- Convenience function to transform a list of definitions and an expression
-- into a single nested let expression.
makeLet :: [Def typeinfo] -> (Expr typeinfo) -> (Expr typeinfo)
makeLet ds e = foldr ((.) . letExpr) id ds e

-- Returns true if the definition is simple, that is, it only contains simple
-- assignments, no pattern matching.
simpleDef :: Def typeinfo -> Bool
simpleDef (VarPat _, _) = True
simpleDef _ = False

-- Returns the set of variables that occur free in the expression.
freeVariables :: Expr typeinfo -> Set Symbol
freeVariables e = case out e of
                    (VarExpr s) -> Set.singleton s
                    (ConstExpr c) -> Set.empty
                    (AppExpr e f) -> Set.union (freeVariables e) (freeVariables f)
                    (AbstrExpr p b) -> freeVariables b `Set.difference` patVariables p
                    (LetExpr (pat, a) e) -> (freeVariables a `Set.union` freeVariables e)
                                           `Set.difference`
                                            patVariables pat
                    (LetrecExpr defs e) ->
                        let (pats, exprs) = unzip defs
                            patVarUnions = scanl1 Set.union (map patVariables pats)
                            defFreeVars = zipWith Set.difference (map freeVariables exprs) patVarUnions
                        in Set.unions defFreeVars
                           `Set.union`
                           (freeVariables e `Set.difference` last patVarUnions)
       
                    (FatbarExpr e f) -> Set.union (freeVariables e) (freeVariables f)
                    (CaseExpr s cases)
                        -> freeVariables s
                           `Set.union`
                            Set.unions [freeVariables exp
                                        `Set.difference`
                                         patVariables pat | (pat, exp) <- cases]

-- Systematically generates all symbols composed of lowercase letters.
symbols :: [Symbol]
symbols = map (:"") ['a'..'z'] ++ ( flip (:) <$> symbols <*> ['a'..'z'] )

-- Systematically generates all symbols composed of lowercase letters which do not occur
-- free in the expression.
nonfreeSymbols :: Expr typeinfo -> [Symbol]
nonfreeSymbols e = filter (`Set.notMember` freeVariables e) symbols

-- Get a variable that does not occur free in the expression.
getNewVariable :: Expr typeinfo -> Symbol
getNewVariable = head . nonfreeSymbols

-- Get multiple variables that does not occur free in the expression.
getNewVariables :: Expr typeinfo -> Int -> [Symbol]
getNewVariables expr n = take n (nonfreeSymbols expr)
