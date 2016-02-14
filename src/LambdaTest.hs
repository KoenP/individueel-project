module LambdaTest where

import Lambda
import LambdaParse
import LambdaShow
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative

instance Arbitrary Expr where
    arbitrary = sized expr
        where
          arbitrarySymbol = elements $ map (:"") ['a'..'z']
          expr 0 = fmap Var $ arbitrarySymbol
          expr n = oneof [ liftM2 App subexpr subexpr
                         , liftM2 Abstr arbitrarySymbol subexpr]
              where subexpr = join $ liftM expr $ choose (0, n `div` 2)
              


prop_freeVariablesAreVariables e = freeVariables e `Set.isSubsetOf` variables e

prop_etaNeverNormalizes e = isNormalForm e == isNormalForm (etaReduce e)

prop_toNormalFormFullyReduces e = isNormalForm $ toNormalForm e
                                   
prop_toNormalFormPreservesMeaning e f = toNormalForm (App e f)
                                        `alphaConvertible`
                                        toNormalForm (App (toNormalForm e) f)

prop_identicalExpressionsAlwaysAlphaConvertible e = e `alphaConvertible` e

prop_alphaReducePreservesMeaning e = all (alphaConvertible (toNormalForm e))
                                         [ toNormalForm (alphaReduce e)
                                         , alphaReduce (toNormalForm e)]
                                     
prop_showSimpleAndParse e = let (Right f) = parseLambdaExpr $ showSimple e
                            in e == f

prop_showExprAndParse e = let (Right f) = parseLambdaExpr $ showExpr e
                          in e == f
