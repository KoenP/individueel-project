module Reduce where

import Control.Applicative ((<$>), (<*>))
import qualified ReductionEngine as RE
import qualified Lambda as L
import Constant

reduce :: L.Expr -> IO ()
reduce (L.Var x) = undefined
reduce (L.Const c) = undefined

buildGraph :: L.Expr -> IO RE.CellPtr
buildGraph (L.Var sym) = RE.makeVar sym
buildGraph (L.Const c) = buildGraphFromConstant c
buildGraph (L.Abstr sym body) = buildGraph body >>= RE.makeAbstr sym
buildGraph (L.App e f) = do
  eGraph <- buildGraph e
  fGraph <- buildGraph f
  RE.makeApp eGraph fGraph

  
buildGraphFromConstant :: Constant -> IO RE.CellPtr
buildGraphFromConstant (IntConst i) = RE.makeNumber i
buildGraphFromConstant c = RE.makeBuiltin (mapBuiltin c)


--typedef enum {PLUS = 0,
--	      MINUS = 1,
--	      YCOMB = 2,
--	      SELECT = 3,
mapBuiltin :: Constant -> Int
mapBuiltin (IntConst i) = undefined
mapBuiltin PlusConst = 0
mapBuiltin MinusConst = 1
mapBuiltin YCombConst = 2
mapBuiltin SelectConst = 3
