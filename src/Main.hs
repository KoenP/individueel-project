module Main where

import EnrichedLambda
import EnrichedLambdaParse
import LambdaShow
import Impoverish

main :: IO ()
main = do
  elce <- fmap parseExpr getLine
  case elce of (Left err) -> print err
               (Right e)  -> putStr $ (++"\n") $ showExpr $ impoverish $ e
  main
