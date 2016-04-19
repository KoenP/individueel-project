module Reduce where

import Control.Applicative ((<$>), (<*>))
import qualified ReductionEngine as RE
import qualified Lambda as L
import Constant

reduce :: L.Expr -> IO ()
reduce expr = buildGraph expr >>= RE.reduce >>= RE.printCell

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
buildGraphFromConstant (ConstrConst (DataTag tag) size) = RE.makeConstructor tag size
buildGraphFromConstant c = RE.makeBuiltin (mapBuiltin c)


--typedef enum {PLUS = 0,
--	      MINUS = 1,
--	      YCOMB = 2,
--	      SELECT = 3,
mapBuiltin :: Constant -> Int
mapBuiltin PlusConst = 0
mapBuiltin MinusConst = 1
mapBuiltin YCombConst = 2
mapBuiltin SelectConst = 3
mapBuiltin EqConst = 4
