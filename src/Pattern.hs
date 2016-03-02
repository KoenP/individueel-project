module Pattern where
import Constant
import Symbol

-- Patterns occur in some enriched lambda calculus expressions.
-- They are either variables, constants, or constructors recursively combining 
-- patterns.
data Pattern typeinfo = VarPat    Symbol
                      | ConstPat  Constant
                      | ConstrPat typeinfo Symbol [Pattern]
                        deriving (Show, Eq)

data ConstrType = SumConstr | ProductConstr deriving (Show, Eq)
