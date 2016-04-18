module Constant where
import Test.QuickCheck

-- Describes a data type that represents constants and built-in functions.
-- Can be used in LC, ELC or Haskell expressions.
data Constant = IntConst Int
              | PlusConst
              | MinusConst
              | YCombConst
              | SelectConst
                deriving (Show, Eq)

-- Translate the constant to a symbol as it occurs in (enriched)
-- lambda calculus.
showConstant :: Constant -> String
showConstant (IntConst n) = show n
--showConstant PlusConst    = "+"
--showConstant IfConst      = "IF"
--showConstant FailConst    = "FAIL"

-- Arbitrary instance for QuickCheck.
instance Arbitrary Constant where
    arbitrary = oneof [ fmap IntConst arbitrary
--                      , pure PlusConst
--                      , pure IfConst
--                      , pure FailConst
                      ]
