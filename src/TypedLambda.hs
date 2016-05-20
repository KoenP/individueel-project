module TypedLambda where
import Symbol
import Pattern
import Constant
import TypeDef
import Fix
import qualified EnrichedLambda as EL
import Control.Monad

-- The typed lambda calculus is a language where a program is defined by a list of
-- type definitions followed by a single enriched lambda expression, where each
-- subexpression may or may not be annotated with a type.

type Type = ()

data TypedProgram typeinfo = TypedProgram [TypeDef] (TypedExpr typeinfo) deriving Show
type TypedExpr typeinfo = EL.AnnExpr typeinfo TypeAnnotation
type TypeAnnotation = Maybe Type
