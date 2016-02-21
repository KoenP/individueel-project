module EnrichedLambdaShow where
import EnrichedLambda
import Constant
import Data.List (intersperse)

-- This simple function's output exactly matches the internal representation
-- of the lambda expression, ie Abstrs have only 1 parameter etc.
showSimple :: Expr -> String
showSimple (VarExpr x)       = x
showSimple (ConstExpr k)     = showConstant k
showSimple (AbstrExpr p b)   = "\\" ++ showPattern p ++ "." ++ showSimple b
showSimple (AppExpr f x)     = "(" ++ showSimple f ++ ") (" ++ showSimple x ++ ")"
showSimple (LetExpr d e)     = showLet showSimple d e
showSimple (LetrecExpr ds e) = showLetRec showSimple ds e
showSimple (FatbarExpr e f)  = showFatbar showSimple e f
showSimple (CaseExpr s es)   = showCase showSimple s es

-- Returns a string representing the expression.
-- Attempts to use a minimal number of parens for easier readability.
-- Function application associates to the left.
-- The body of an abstraction extends as far right as possible.
showExpr :: Expr -> String
showExpr (VarExpr x)       = x
showExpr (ConstExpr k)     = showConstant k
showExpr e@(AbstrExpr _ _) = let (patterns, body) = flattenNestedAbstrExprs e
                             in "\\" ++ unwords (map showPattern patterns)
                                     ++ "." ++ showExpr body
showExpr e@(AppExpr _ _ )  = let terms = flattenNestedAppExprs e
                                 showTerm t = case t
                                              of VarExpr x -> x
                                                 e -> "(" ++ showExpr e ++ ")"
                             in unwords (map showTerm terms)
showExpr (LetExpr d e)     = showLet showExpr d e
showExpr (LetrecExpr ds e) = showLetRec showExpr ds e
showExpr (FatbarExpr e f)  = showFatbar showExpr e f
showExpr (CaseExpr s es)   = showCase showExpr s es

-- IO action to display an expression on stdout.
printExpr :: Expr -> IO ()
printExpr = putStrLn . showExpr

-- Produces a string representing a simple let expression.
-- Does not simplify nested lets.
-- Needs to be provided with a function to transform expressions to strings.
showLet :: (Expr -> String) -> Def -> Expr -> String
showLet showF d e = "let " ++ showDefinition showF d
                           ++ " in " ++ showF e

-- Produces a string representing a recursive let expression.
-- Needs to be provided with a function to transform expressions to strings.
showLetRec :: (Expr -> String) -> [Def] -> Expr -> String
showLetRec showF ds e
    = "letrec " ++ concatMap ((++", ") . showDefinition showF) ds
                ++ " in " ++ showF e

-- Produces a string representing a fatbar ([]) expression.
-- Needs to be provided with a function to transform expressions to strings.
showFatbar :: (Expr -> String) -> Expr -> Expr -> String
showFatbar showF e f = showF e ++ " [] " ++ showF f

-- Produces a string representing a case ([]) expression.
-- Needs to be provided with a function to transform expressions to strings.
showCase :: (Expr -> String) -> Symbol -> [(Pattern, Expr)] -> String
showCase showF s es = "case " ++ s ++ " of "
                              ++ concatMap (showCaseEntry showF) es

-- Shows a definition as "<pattern> = <expr>".
-- A function must be provided to show the expression.
showDefinition :: (Expr -> String) -> Def -> String
showDefinition showF (p, e) = showPattern p ++ " = " ++ showF e

-- Shows an entry in a case expression as "<pattern> -> <expr>;"
-- A function must be provided to show the expression.
showCaseEntry :: (Expr -> String) -> (Pattern, Expr) -> String
showCaseEntry showF (p, e) = showPattern p ++ " -> " ++ showF e ++ "; "

-- Shows a pattern.
-- If the pattern is a variable, it simply returns the symbol.
-- TODO: constructor pattern.
showPattern :: Pattern -> String
showPattern (VarPat s)       = s
showPattern (ConstPat k)     = showConstant k
showPattern (ConstrPat s ps) = "(" ++ unwords (s : map showPattern ps) ++ ")"

-- Helper function for showExpr.
-- If several abstractions are nested, it returns all parameters as a
-- list of symbols, and it returns the body of the most deeply nested
-- abstraction.
-- It is used to generate a more easily human-readable representation
-- of abstractions.
flattenNestedAbstrExprs :: Expr -> ([Pattern], Expr)
flattenNestedAbstrExprs (AbstrExpr p b) = let (ps, b') = flattenNestedAbstrExprs b
                                           in (p:ps, b')
flattenNestedAbstrExprs e           = ([], e)

-- Similar to flattenNestedAbstrExprs, but for applications.
flattenNestedAppExprs :: Expr -> [Expr]
flattenNestedAppExprs (AppExpr f x) = let terms = flattenNestedAppExprs f 
                                      in terms ++ [x]
flattenNestedAppExprs e             = [e]
