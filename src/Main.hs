module Main where

import EnrichedLambda
import EnrichedLambdaParse
import LambdaShow
import Impoverish
import System.IO

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  elce <- fmap parseExpr getLine
  case elce of (Left err) -> print err
               (Right e)  -> putStr $ (++"\n") $ showExpr $ impoverish $ e
  putChar '\n'
  hFlush stdout
  main
