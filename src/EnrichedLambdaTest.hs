module EnrichedLambdaTest where

import EnrichedLambda
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative

-- NOTE: DOES NOT YET GENERATE CONSTANTS!
instance Arbitrary Expr where
    arbitrary = sized expr
        where
          arbitrarySymbol = elements $ map (:"") ['a'..'z']
          expr 0 = fmap fromSymbol arbitrarySymbol
          expr n = oneof [ liftM2 AppExpr subexpr subexpr
                         , liftM2 AbstrExpr arbitrarySymbol subexpr
                         ]
              where subexpr = join $ liftM expr $ choose (0, n `div` 2)

prop_showSimpleAndParse e = let (Right f) = parseExpr $ showSimple e
                            in e == f

prop_showExprAndParse e = let (Right f) = parseExpr $ showExpr e
                          in e == f
