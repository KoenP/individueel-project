module EnrichedLambda where
import Symbol
import Pattern
import Constant
import TypeDef
import Control.Monad (join)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set

-- An expression in the enriched lambda calculus.
data Expr typeinfo = VarExpr    Symbol
                   | ConstExpr  Constant
                   | AppExpr    (Expr typeinfo)     (Expr typeinfo)
                   | AbstrExpr  (Pattern typeinfo)  (Expr typeinfo)
                   | LetExpr    (Def typeinfo)      (Expr typeinfo)
                   | LetrecExpr [Def typeinfo]      (Expr typeinfo)
                   | FatbarExpr (Expr typeinfo)     (Expr typeinfo)
                   | CaseExpr   (Expr typeinfo)     [(Pattern typeinfo, Expr typeinfo)]
                   | HasType    (Expr typeinfo)     Type
                     deriving (Show, Eq)

type Def typeinfo = (Pattern typeinfo, Expr typeinfo) -- Definition

applyToChildren :: (Expr typeinfo -> Expr typeinfo) -> Expr typeinfo -> Expr typeinfo
applyToChildren f (AppExpr e1 e2)     = AppExpr (f e1) (f e2)
applyToChildren f (AbstrExpr x e)     = AbstrExpr x (f e)
applyToChildren f (LetExpr def e)     = LetExpr def (f e)
applyToChildren f (LetrecExpr defs e) = LetrecExpr defs (f e)
applyToChildren f (FatbarExpr e1 e2)  = FatbarExpr (f e1) (f e2)
applyToChildren f (CaseExpr e cs)     = CaseExpr (f e) [(p, f e') | (p, e') <- cs]
applyToChildren f (HasType e t)       = HasType (f e) t

-- Given an ELC expression with no type information, return the expression
-- with type information.
fillInTypeInfo :: (Symbol -> Maybe TypeDef)
               -> Expr ()
               -> Either ConstructorUndefinedError (Expr TypeDef)
fillInTypeInfo g e =
    case e of
      (VarExpr x)         -> pure (maybe (VarExpr x) ConstExpr (getConstrConstant g x))
      (ConstExpr k)       -> pure (ConstExpr k)
      (AppExpr e f)       -> AppExpr <$> h e <*> h f
      (AbstrExpr pat e)   -> AbstrExpr <$> fillInPatTypeInfo g pat <*> h e
      (LetExpr def e)     -> LetExpr <$> fillInDefTypeInfo def <*> h e
      (LetrecExpr defs e) -> LetrecExpr <$> sequence (map fillInDefTypeInfo defs) <*> h e
      (FatbarExpr e f)    -> FatbarExpr <$> h e <*> h f
      (CaseExpr s cases)
          -> CaseExpr
             <$> fillInTypeInfo g s
             <*> sequence
                 (map (\(p,e)->(,) <$> fillInPatTypeInfo g p <*> h e) cases)
      (HasType e t)       -> HasType <$> h e <*> pure t
    where
      h = fillInTypeInfo g
      fillInDefTypeInfo :: Def () -> Either ConstructorUndefinedError (Def TypeDef)
      fillInDefTypeInfo (p,e) = (,) <$> fillInPatTypeInfo g p <*> h e

getConstrConstant :: (Symbol -> Maybe TypeDef) -> Symbol -> Maybe Constant
getConstrConstant f s = join $ getFromTypeDef <$> f s
    where
      getFromTypeDef :: TypeDef -> Maybe Constant
      getFromTypeDef (TypeDef _ _ constrs) = do
        (index, constr) <- find ((==s) . getConstrName . snd) (zip [0..] constrs)
        return $ ConstrConst (DataTag index) (length $ getConstrFields constr)

dropTypeAnnotations :: Expr typeinfo -> Expr typeinfo
dropTypeAnnotations (HasType e _) = e
dropTypeAnnotations e = applyToChildren dropTypeAnnotations e

-- Convenience function to transform a list of patterns and an expression
-- into a single nested abstraction.
makeAbstr :: [Pattern typeinfo] -> (Expr typeinfo) -> (Expr typeinfo)
makeAbstr ps e = foldr ((.) . AbstrExpr) id ps e

-- Convenience function for when you want to make a set of nested applications
-- from a list of expressions containing at least two expressions.
makeApp :: (Expr typeinfo) -> (Expr typeinfo) -> [(Expr typeinfo)] -> (Expr typeinfo)
makeApp e1 e2 es = foldl AppExpr (AppExpr e1 e2) es

-- Convenience function to transform a list of definitions and an expression
-- into a single nested let expression.
makeLet :: [Def typeinfo] -> (Expr typeinfo) -> (Expr typeinfo)
makeLet ds e = foldr ((.) . LetExpr) id ds e

-- Returns true if the definition is simple, that is, it only contains simple
-- assignments, no pattern matching.
simpleDef :: Def typeinfo -> Bool
simpleDef (VarPat _, _) = True
simpleDef _ = False

-- Returns the set of variables that occur free in the expression.
freeVariables :: Expr typeinfo -> Set Symbol
freeVariables (VarExpr s) = Set.singleton s
freeVariables (ConstExpr c) = Set.empty
freeVariables (AppExpr e f) = Set.union (freeVariables e) (freeVariables f)
freeVariables (AbstrExpr p b) = freeVariables b `Set.difference` patVariables p
freeVariables (LetExpr (pat, a) e) = (freeVariables a `Set.union` freeVariables e)
                                     `Set.difference`
                                     patVariables pat
freeVariables (LetrecExpr defs e) = 
    let (pats, exprs) = unzip defs
        patVarUnions = scanl1 Set.union (map patVariables pats)
        defFreeVars = zipWith Set.difference (map freeVariables exprs) patVarUnions
    in Set.unions defFreeVars
           `Set.union` (freeVariables e `Set.difference` last patVarUnions)
       
freeVariables (FatbarExpr e f) = Set.union (freeVariables e) (freeVariables f)
freeVariables (CaseExpr s cases)
    = freeVariables s `Set.union` Set.unions [freeVariables exp
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
