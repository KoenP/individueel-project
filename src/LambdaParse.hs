module LambdaParse where
import Lambda
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

--------------------------------------------------------------------------------
-- GRAMMAR
--   expr  ::= var | app | abstr | ( expr )
--   app   ::= expr expr expr*
--   abstr ::= \ var var* . expr
--------------------------------------------------------------------------------

-- Parses a String into a lambda expression, using the grammar described above.
-- There are multiple valid notations for the lambda calculus.
-- This parser adheres to the notation used in
--   "The Implementation of Functional Programming Languages".
parseLambdaExpr :: String -> Either ParseError Expr
parseLambdaExpr = parse exprParser ""

exprParser :: Parser Expr
exprParser = m_whiteSpace >> parseSubExpr <* eof
    where
      parseSubExpr =     (parseAbstr            <?> "abstraction"               )
                     <|> (try parseApp          <?> "application"               )
                     <|> (parseVar              <?> "variable"                  )
                     <|> (m_parens parseSubExpr <?> "parenthethised expression" )

      parseAbstr = do m_reservedOp "\\"
                      symbols <- many1 m_identifier
                      m_reservedOp "."
                      body    <- parseSubExpr
                      return $ foldr ((.) . Abstr) id symbols body

      parseAppTerm = parseVar <|> m_parens parseSubExpr
      parseApp = do terms <- many1 parseAppTerm
                    return $ foldl1 App terms

      parseVar = fmap Var m_identifier

ldef :: LanguageDef st
ldef = emptyDef { identStart      = alphaNum <|> oneOf "+-*/="
                , identLetter     = alphaNum <|> oneOf "+-*/="
                , reservedOpNames = ["\\", "."]
                }

TokenParser { parens     = m_parens
            , identifier = m_identifier
            , reservedOp   = m_reservedOp
            , whiteSpace = m_whiteSpace } = makeTokenParser ldef
