module Main where

import qualified Lambda as L
import Constant
import EnrichedLambda
import EnrichedLambdaParse
import LambdaShow
import Impoverish
import TypeDef
import Symbol
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Data.Bifunctor (first)
import qualified ReductionEngine as RE
import Reduce
import Generic.Varia (showError, showEitherResult)
import LambdaViz

data RunMode = Compile FilePath
             | Execute FilePath
             | Repl
             | Dot

type ConstructorLookup = Symbol -> Maybe TypeDef

main :: IO ()
main = do
  runMode <- fmap parseArgs getArgs
  typedefs <- readTypeDefinitions "typedefs"
  run (getConstructorType typedefs) runMode
      

run :: ConstructorLookup -> RunMode -> IO ()
--run cl Dot = interact (lambdaToDot
--                       . impoverish
--                       . stripError
--                       . fillInTypeInfo cl
--                       . stripError
--                       . parseExpr)
run cl Repl = repl cl (\e -> print e >> lambdaViz e >> reduce e)
       
repl :: ConstructorLookup -> (L.Expr -> IO ()) -> IO ()
repl constrLookup process = do
        putStr "> "
        hFlush stdout
        elce <- fmap (showError . parseExpr) getLine
        let impoverished = elce >>= (showError
                                    . fmap impoverish
                                    . fillInTypeInfo constrLookup)
        (case impoverished of Left err -> putStr err
                              Right e  -> process e)
        putChar '\n'
        hFlush stdout
        repl constrLookup process

parseArgs :: [String] -> RunMode
parseArgs ["-x", src] = Execute src
parseArgs ["-d"] = Dot
parseArgs ["--dot"] = Dot
parseArgs _ = Repl

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

stripError :: (Show a) => Either a b -> b
stripError (Left a) = error $ show a
stripError (Right b) = b
