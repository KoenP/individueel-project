module EnrichedLambda where
    
-- An expression in the enriched lambda calculus.
data Expr = VarExpr    Symbol
          | AppExpr    Expr Expr
          | AbstrExpr  Symbol Expr
          | LetExpr    Def Expr
          | LetrecExpr [Def] Expr
          | FatbarExpr Expr Expr
          | CaseExpr   Symbol [(Pattern, Expr)]
            deriving (Show, Eq)

-- Patterns occur in some enriched lambda calculus expressions.
-- They are either variables, constants, or constructors recursively combining 
-- patterns.
data Pattern = VarPat    Symbol
             | ConstrPat [Pattern]
               deriving (Show, Eq)

type Def = (Pattern, Expr) -- Definition
type Symbol = String

fromSymbol :: Symbol -> Expr
fromSymbol = VarExpr

--------------------------------------------------------------------------------
-- OUTPUT ----------------------------------------------------------------------
--------------------------------------------------------------------------------
       
-- Returns a string representing the expression.
-- This simple function's output exactly matches the internal representation
-- of the lambda expression, ie Abstrs have only 1 parameter etc.
showSimple :: Expr -> String
showSimple (VarExpr x) = x
showSimple (AbstrExpr s b) = "\\" ++ s ++ "." ++ showSimple b
showSimple (AppExpr f x) = "(" ++ showSimple f ++ ") (" ++ showSimple x ++ ")"

-- Returns a string representing the expression.
-- Attempts to use a minimal number of parens for easier readability.
-- Function application associates to the left.
-- The body of an abstraction extends as far right as possible.
showExpr :: Expr -> String
showExpr (VarExpr x) = x
showExpr e@(AbstrExpr _ _) = let (symbols, body) = flattenNestedAbstrExprs e
                             in "\\" ++ unwords symbols ++ "." ++ showExpr body
showExpr e@(AppExpr _ _ ) = let terms = flattenNestedAppExprs e
                                showTerm t = case t
                                             of VarExpr x -> x
                                                e -> "(" ++ showExpr e ++ ")"
                            in unwords (map showTerm terms)

-- Helper function for showExpr.
-- If several abstractions are nested, it returns all parameters as a
-- list of symbols, and it returns the body of the most deeply nested
-- abstraction.
-- It is used to generate a more easily human-readable representation
-- of abstractions.
flattenNestedAbstrExprs :: Expr -> ([Symbol], Expr)
flattenNestedAbstrExprs (AbstrExpr s b) = let (ss, b') = flattenNestedAbstrExprs b
                                  in (s:ss, b')
flattenNestedAbstrExprs e           = ([], e)

-- Similar to flattenNestedAbstrExprs, but for applications.
flattenNestedAppExprs :: Expr -> [Expr]
flattenNestedAppExprs (AppExpr f x) = let terms = flattenNestedAppExprs f 
                              in terms ++ [x]
flattenNestedAppExprs e         = [e]

-- IO action to display an expression on stdout.
printExpr :: Expr -> IO ()
printExpr = putStrLn . showExpr
