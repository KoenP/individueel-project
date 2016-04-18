module Main where

import qualified Lambda as L
import Constant
import EnrichedLambda
import EnrichedLambdaParse
import LambdaShow
import Impoverish
import TypeDef
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Bifunctor (first)
import qualified ReductionEngine as RE
import Reduce

main :: IO ()
main = let expr = L.Const (IntConst 4)
       in buildGraph expr


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

