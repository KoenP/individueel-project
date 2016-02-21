module EnrichedLambdaTest where

import EnrichedLambda
import EnrichedLambdaParse
import EnrichedLambdaShow
import Constant
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative

-- Generates a single arbitrary one-letter lowercase symbol.
arbitrarySymbol :: Gen Symbol
arbitrarySymbol = elements $ map (:"") ['a'..'z']

-- Generates an arbitrary smaller substructure in a recursive structure.
subStructure :: Arbitrary s => (Int -> Gen s) -> Int -> Int -> Gen s
subStructure f n d = join $ liftM f $ choose (0, n `div` d)

printArbitraryExpr :: IO ()
printArbitraryExpr = do e <- generate (arbitrary :: Gen Expr)
                        print e
                        putStrLn $ showExpr e
                        putStrLn $ showSimple e

instance Arbitrary Expr where
    arbitrary = sized expr
        where
          expr 0 = oneof [ fmap VarExpr arbitrarySymbol
                         , fmap ConstExpr arbitrary
                         ]
          expr n = oneof [ liftM2 AppExpr subexpr subexpr
                         , liftM2 AbstrExpr arbitrary subexpr
                         , liftM2 LetExpr ((,) <$> arbitrary <*> subexpr) subexpr
                         ]
              where subexpr = subStructure expr n 2

instance Arbitrary Pattern where
    arbitrary = sized pat
        where
          pat 0 = oneof [ fmap VarPat arbitrarySymbol
                        , fmap ConstPat arbitrary
                        ]
          pat n = do m <- choose (1, 3)
                     let subpat = subStructure pat n (m+1)
                     pats <- replicateM m subpat
                     s <- arbitrarySymbol
                     return (ConstrPat s pats)
prop_showSimpleAndParse e = let (Right f) = parseExpr $ showSimple e
                            in e == f

prop_showExprAndParse e = let (Right f) = parseExpr $ showExpr e
                          in e == f
