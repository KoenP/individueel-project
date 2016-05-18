module TypedLambda where
import Symbol
import Pattern
import Constant
import TypeDef
import qualified EnrichedLambda as EL
import Control.Monad

-- The typed lambda calculus is a language where a program is defined by a list of
-- type definitions followed by a single enriched lambda expression.
data TypedExpr typeinfo = TypedExpr [TypeDef] (EL.Expr typeinfo)


