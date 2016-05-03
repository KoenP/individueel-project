module EnrichedLambdaTest where

import qualified Lambda
import qualified LambdaTest
import Pattern
import Symbol
import Impoverish
import TypeDef (Constructor, getConstrName, getConstrFields)
import qualified TypeDef as TD
import EnrichedLambda
import EnrichedLambdaParse
import EnrichedLambdaShow
import Constant
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative
import Data.Char (toUpper)

-- Generates a single arbitrary one-letter lowercase symbol.
arbitrarySymbol :: Gen Symbol
arbitrarySymbol = elements $ map (:"") ['a'..'z']

-- Generates an arbitrary smaller substructure in a recursive structure.
subStructure :: Arbitrary s => (Int -> Gen s) -> Int -> Int -> Gen s
subStructure f n d = join $ liftM f $ choose (0, n `div` d)

printArbitraryExpr :: IO ()
printArbitraryExpr = do e <- generate (arbitrary :: Gen (Expr TD.TypeDef))
                        print e
                        putStrLn $ showExpr e
                        putStrLn $ showSimple e

class TypeDefinition td where
    getTypeName :: td -> Symbol
    getTypeConstructors :: td -> [Constructor]

instance TypeDefinition TD.TypeDef where
    getTypeName = TD.getTypeName
    getTypeConstructors = TD.getTypeConstructors

instance (Arbitrary td, TypeDefinition td) => Arbitrary (Expr td) where
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

instance (Arbitrary td, TypeDefinition td) => Arbitrary (Pattern td) where
    arbitrary = sized pat
        where
          pat 0 = oneof [ fmap VarPat arbitrarySymbol
                        , fmap ConstPat arbitrary
                        ]
          pat n = do typedef <- arbitrary
                     constr <- oneof . map return . getTypeConstructors $ typedef
                     let symbol = getConstrName constr
                     pats <- vector . length . getConstrFields $ constr
                     return (ConstrPat typedef symbol pats)

instance Arbitrary TD.TypeDef where
    arbitrary = TD.TypeDef <$> arbitrarySymbol <*> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary Constructor where
    arbitrary = TD.Constructor <$> fmap (map toUpper) arbitrarySymbol
                               <*> arbitrary

prop_showSimpleAndParse e = let (Right f) = parseExpr $ showSimple e
                            in e == f

prop_showExprAndParse e = let (Right f) = parseExpr $ showExpr e
                          in e == f

prop_freeVariablesTerminates e = freeVariables (e :: Expr TD.TypeDef) `seq` True
prop_freeVariablesSameAsLambda e = freeVariables e == Lambda.freeVariables (impoverish e)
