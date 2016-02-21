module EnrichedLambda where
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
