module EnrichedLambda where
import Pattern
import Constant

-- XXX Constants not yet implemented.
    
-- An expression in the enriched lambda calculus.
data Expr = VarExpr    Symbol
          | ConstExpr  Constant
          | AppExpr    Expr     Expr
          | AbstrExpr  Pattern  Expr
          | LetExpr    Def      Expr
          | LetrecExpr [Def]    Expr
          | FatbarExpr Expr     Expr
          | CaseExpr   Symbol   [(Pattern, Expr)]
            deriving (Show, Eq)

-- Patterns occur in some enriched lambda calculus expressions.
-- They are either variables, constants, or constructors recursively combining 
-- patterns.
data Pattern = VarPat    Symbol
             | ConstPat  Constant
             | ConstrPat Symbol [Pattern]
               deriving (Show, Eq)

type Def = (Pattern, Expr) -- Definition
type Symbol = String

-- Convenience function to transform a list of patterns and an expression
-- into a single nested abstraction.
makeAbstr :: [Pattern] -> Expr -> Expr
makeAbstr ps e = foldr ((.) . AbstrExpr) id ps e

-- Convenience function for when you want to make a set of nested applications
-- from a list of expressions containing at least two expressions.
makeApp :: Expr -> Expr -> [Expr] -> Expr
makeApp e1 e2 es = foldl AppExpr (AppExpr e1 e2) es

-- Convenience function to transform a list of definitions and an expression
-- into a single nested let expression.
makeLet :: [Def] -> Expr -> Expr
makeLet ds e = foldr ((.) . LetExpr) id ds e

-- Returns true if the definition is simple, that is, it only contains simple
-- assignments, no pattern matching.
simpleDef :: Def -> Bool
simpleDef (VarPat _, _) = True
simpleDef _ = False
