module Constant where
import Test.QuickCheck

-- Describes a data type that represents constants and built-in functions.
-- Can be used in LC, ELC or Haskell expressions.
data Constant = IntConst Int
              | PlusConst
              | MinusConst
              | YCombConst
              | SelectConst
              | ConstrConst DataTag Size
              | EqConst
                deriving (Show, Eq)

newtype DataTag = DataTag Int deriving (Eq)
instance Show DataTag where
    show (DataTag i) = show i
type Size = Int

-- Translate the constant to a symbol as it occurs in (enriched)
-- lambda calculus.
showConstant :: Constant -> String
showConstant (IntConst n) = show n
showConstant PlusConst    = "+"
showConstant MinusConst = "-"
showConstant YCombConst = "Y"
showConstant SelectConst = "SELECT"
showConstant (ConstrConst (DataTag i) size) = "CONSTR-" ++ show i ++ "-" ++ show size
showConstant EqConst = "="

-- Arbitrary instance for QuickCheck.
instance Arbitrary Constant where
    arbitrary = oneof [ fmap IntConst arbitrary
--                      , pure PlusConst
--                      , pure IfConst
--                      , pure FailConst
                      ]
