module EnrichedLambda where
import Symbol
import Pattern
import Constant
import TypeDef
import Control.Monad (join)
import Data.List (find)

-- XXX Constants not yet implemented.
    
-- An expression in the enriched lambda calculus.
data Expr typeinfo = VarExpr    Symbol
                   | ConstExpr  Constant
                   | AppExpr    (Expr typeinfo)     (Expr typeinfo)
                   | AbstrExpr  (Pattern typeinfo)  (Expr typeinfo)
                   | LetExpr    (Def typeinfo)      (Expr typeinfo)
                   | LetrecExpr [Def typeinfo]      (Expr typeinfo)
                   | FatbarExpr (Expr typeinfo)     (Expr typeinfo)
                   | CaseExpr   Symbol              [(Pattern typeinfo, Expr typeinfo)]
                     deriving (Show, Eq)

type Def typeinfo = (Pattern typeinfo, Expr typeinfo) -- Definition

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
          -> CaseExpr s
             <$> sequence
                 (map (\(p,e)->(,) <$> fillInPatTypeInfo g p <*> h e) cases)
    where
      h = fillInTypeInfo g
      fillInDefTypeInfo :: Def () -> Either ConstructorUndefinedError (Def TypeDef)
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
