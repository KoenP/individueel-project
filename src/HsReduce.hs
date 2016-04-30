import Symbol
import Constant
import Lambda
import Generic.Glutton
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.IORef

data Cell = VarCell Symbol
          | AppCell CellRef CellRef
          | AbstrCell Symbol CellRef
          | DataCell DataTag () -- TODO
          | BuiltinCell Constant
          | ConstrCell DataTag Int
                 
type CellRef = IORef Cell

--------------------------------------------------------------------------------
-- GraphViz visualisation.
--------------------------------------------------------------------------------

-- Gaat niet werken, ctr wordt niet recursief doorgegeven
gvNodesAndEdges :: CellRef -> IO ([LNode Symbol], [LEdge Symbol])
gvNodesAndEdges ref = do
  cell <- readIORef ref
  counter <- newIORef (0 :: Int)
  f cell counter
    where
      f (VarCell sym) ctr = do id <- getId ctr
                               return ([(id, sym)], [])
      f (AppCell e1 e2) ctr = do
                        (e1Root:e1Nodes, e1Edges) <- gvNodesAndEdges e1
                        (e2Root:e2Nodes, e2Edges) <- gvNodesAndEdges e2
                        id <- getId ctr
                        let nodes = (id, "@") : (e1Root:e1Nodes) ++ (e2Root:e2Nodes)
                            edges = (id,fst e1Root,"") : (id,fst e2Root,"")
                                                       : (e1Edges ++ e2Edges)
                        return (nodes, edges)
      f (AbstrCell s b) ctr = do
                        (bRoot:bNodes, bEdges) <- gvNodesAndEdges b
                        id <- getId ctr
                        let nodes = (id, "λ"++s) : (bRoot:bNodes)
                            edges = (id, fst bRoot, "") : bEdges
                        return (nodes, edges)
      getId ctr = do ct <- readIORef ctr
                     modifyIORef' ctr (+1)
                     return ct

--------------------------------------------------------------------------------
-- Make cells.
--------------------------------------------------------------------------------
buildLambdaGraph :: Expr -> IO CellRef
buildLambdaGraph (Var sym) = makeVarCell sym
buildLambdaGraph (Const c) = makeBuiltinCell c
buildLambdaGraph (Abstr sym body) = buildLambdaGraph body >>= makeAbstrCell sym
buildLambdaGraph (App e f) = do
  eCell <- buildLambdaGraph e
  fCell <- buildLambdaGraph f
  makeAppCell eCell fCell

makeVarCell :: Symbol -> IO CellRef
makeVarCell = newIORef . VarCell
                 
makeAppCell :: CellRef -> CellRef -> IO CellRef
makeAppCell e f = newIORef (AppCell e f)
                 
makeAbstrCell :: Symbol -> CellRef -> IO CellRef
makeAbstrCell sym body = newIORef (AbstrCell sym body)
                 
                 
makeBuiltinCell :: Constant -> IO CellRef
makeBuiltinCell = undefined
