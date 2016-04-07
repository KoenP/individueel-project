module Main where

import EnrichedLambda
import EnrichedLambdaParse
import LambdaShow
import Impoverish
import TypeDef
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Bifunctor (first)
import qualified ReductionEngine as RE

main :: IO ()
main = do
  testReductionEngine

  {-
  -- Read type definition file.
  [typedefFilePath] <- getArgs
  typedefs <- readTypeDefinitions typedefFilePath
  let constrLookup = getConstructorType typedefs

  repl constrLookup
  where repl constrLookup = do
          putStr "> "
          hFlush stdout
          elce <- fmap parseExpr getLine
          putStr $ either show
                          (either show (showExpr . impoverish) . fillInTypeInfo constrLookup) elce
                   

          putChar '\n'
          hFlush stdout
          repl constrLookup
   -}


readTypeDefinitions :: FilePath -> IO [TypeDef]
readTypeDefinitions fp = do
  input <- fmap parseTypeDef (readFile fp)
  case input of
    (Left err)  -> error (show err)
    (Right tds) -> return tds

readEnrichedLambda :: FilePath -> IO (Expr ())
readEnrichedLambda fp = do
  input <- fmap parseExpr (readFile fp)
  case input of
    (Left err) -> error (show err)
    (Right e)  -> return e


testReductionEngine :: IO ()
testReductionEngine = do
  n1 <- RE.makeNumber 5
  n2 <- RE.makeNumber 12
  app <- RE.makeApp n1 n2
  RE.printCell app
  putStr "\n"
  
