module Pattern where
import Constant
import Symbol
import TypeDef
import Data.Coerce
import Data.Set (Set)
import qualified Data.Set as S

-- Patterns occur in some enriched lambda calculus expressions.
-- They are either variables, constants, or constructors recursively combining 
-- patterns.
data Pattern typeinfo = VarPat    Symbol
                      | ConstPat  Constant
                      | ConstrPat typeinfo Symbol [Pattern typeinfo]
                        deriving (Show, Eq)

-- Add type information to constructor patterns.
fillInPatTypeInfo :: (Symbol -> Maybe TypeDef)
                  -> Pattern ()
                  -> Either ConstructorUndefinedError (Pattern TypeDef)
fillInPatTypeInfo g (ConstrPat () s pats)
    = let h = fillInPatTypeInfo g
      in case g s of
           Nothing   -> Left (ConstructorUndefinedError s)
           (Just td) -> Right (ConstrPat td s) <*> sequence (map h pats)
fillInPatTypeInfo _ (VarPat s)   = Right (VarPat s)
fillInPatTypeInfo _ (ConstPat k) = Right (ConstPat k)

patVariables :: Pattern t -> Set Symbol
patVariables (VarPat s)           = S.singleton s
patVariables (ConstPat _)         = S.empty
patVariables (ConstrPat _ _ pats) = foldl S.union S.empty (map patVariables pats)
