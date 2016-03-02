module LambdaShow where
import Lambda
import Constant

-- Returns a string representing the expression.
-- This simple function's output exactly matches the internal representation
-- of the lambda expression, ie Abstrs have only 1 parameter etc.
showSimple :: Expr -> String
showSimple (Var x) = x
showSimple (Const k) = showConstant k
showSimple (Abstr s b) = "\\" ++ s ++ "." ++ showSimple b
showSimple (App f x) = "(" ++ showSimple f ++ ") (" ++ showSimple x ++ ")"

-- Returns a string representing the expression.
-- Attempts to use a minimal number of parens for easier readability.
-- Function application associates to the left.
-- The body of an abstraction extends as far right as possible.
showExpr :: Expr -> String
showExpr (Var x)       = x
showExpr (Const k)     = showConstant k
showExpr e@(Abstr _ _) = let (symbols, body) = flattenNestedAbstrs e
                         in "\\" ++ unwords symbols
                                ++ "." ++ showExpr body
showExpr e@(App _ _ )  = let terms = flattenNestedApps e
                             showTerm t = case t of Var x -> x
                                                    Const k -> showConstant k
                                                    e     -> "(" ++ showExpr e ++ ")"
                         in unwords (map showTerm terms)

-- Helper function for showExpr.
-- If several abstractions are nested, it returns all parameters as a
-- list of symbols, and it returns the body of the most deeply nested
-- abstraction.
-- It is used to generate a more easily human-readable representation
-- of abstractions.
flattenNestedAbstrs :: Expr -> ([Symbol], Expr)
flattenNestedAbstrs (Abstr s b) = let (ss, b') = flattenNestedAbstrs b
                                  in (s:ss, b')
flattenNestedAbstrs e           = ([], e)

-- Similar to flattenNestedAbstrs, but for applications.
flattenNestedApps :: Expr -> [Expr]
flattenNestedApps (App f x) = let terms = flattenNestedApps f 
                              in terms ++ [x]
flattenNestedApps e         = [e]

-- IO action to display an expression on stdout.
printExpr :: Expr -> IO ()
printExpr = putStrLn . showExpr
