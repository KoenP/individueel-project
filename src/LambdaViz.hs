module LambdaViz where
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Lambda
import Constant
import Generic.Glutton

lambdaViz :: Expr -> IO ()
lambdaViz e = preview (convert e) >> putStrLn "PREVIEW!"

convert :: Expr -> Gr String String
convert e = uncurry mkGraph $ feed (nodesAndEdges e) [0..]

nodesAndEdges :: Expr -> Glutton Node ([LNode String], [LEdge String])
nodesAndEdges (Var x) = nibbler $ \node -> ([(node, x)], [])
nodesAndEdges (Const c) = nibbler $ \node -> ([(node, showConstant c)], [])
nodesAndEdges (Abstr sym body) = do
  (bodyRoot:bodyNodes, bodyEdges) <- nodesAndEdges body 
  node <- nibbler id
  let nodes = (node, "λ"++sym) : (bodyRoot:bodyNodes)
      edges = (node, fst bodyRoot, "") : bodyEdges
  return (nodes, edges)
nodesAndEdges (App e f) = do
  (eRoot:eNodes, eEdges) <- nodesAndEdges e
  (fRoot:fNodes, fEdges) <- nodesAndEdges f
  node <- nibbler id
  let nodes = (node, "@") : (eRoot:eNodes) ++ (fRoot:fNodes)
      edges = (node, fst eRoot, "") : (node, fst fRoot, "") : eEdges ++ fEdges
  return (nodes, edges)
