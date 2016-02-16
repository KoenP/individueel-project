module EnrichedLambdaParse where
import EnrichedLambda
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

--------------------------------------------------------------------------------
-- GRAMMAR: see Implementation of Functional Programming Languages p40.
--------------------------------------------------------------------------------

-- Parses a String into an extended lambda expression, using the grammar described above.
-- There are multiple valid notations for the lambda calculus.
-- This parser adheres to the notation used in
--   "The Implementation of Functional Programming Languages".
-- Function application associates to the left.
-- Abstractions extend as far right as possible.
parseExpr :: String -> Either ParseError Expr
parseExpr = parse exprParser ""

-- Parser for enriched lambda calculus.
-- TODO: parsing patterns.
exprParser :: Parser Expr
exprParser = m_whiteSpace >> expr <* eof
    where
      expr =     (abstr       <?> "abstraction")
             <|> (letExpr     <?> "let expression")
             <|> (letrecExpr  <?> "letrec expression")
             <|> (caseExpr    <?> "case expression")
             <|> (try subexpr <?> "subexpression")
             <|> (var         <?> "variable")

      -- Parses abstractions.
      -- The internal representation only provides single-argument abstractions,
      -- but the notation allows multiple-argument abstractions.
      -- This parser parses multiple-argument abstractions into nested single-
      -- argument abstractions.
      abstr = do m_reservedOp "\\"
                 symbols <- many1 m_identifier
                 m_whiteSpace >> char '.' >> m_whiteSpace
                 body <- expr
                 return $ foldr ((.) . AbstrExpr) id symbols body

      -- Parses a simple let expression.
      letExpr = do m_reserved "let"
                   d <- definition
                   m_reserved "in"
                   e <- expr
                   return (LetExpr d e)
                          
      -- Parses a recursive let expression.
      -- There is a deviation from the book notation here: definitions
      -- are separated by a comma (,).
      letrecExpr = do m_reserved "letrec"
                      ds <- sepBy1 definition (m_reservedOp ",")
                      m_reserved "in"
                      e <- expr
                      return (LetrecExpr ds e)

      -- Parses a definition. This is used in parsing let
      -- and letrec expressions.
      definition = do p <- pattern
                      m_reservedOp "="
                      e <- expr
                      return (p, e)

      -- Parses a case expression.
      -- Deviation from book notation: all pattern-expression pairs must
      -- be terminated by a semicolon (;).
      caseExpr = do m_reserved "case"
                    s <- m_identifier
                    m_reserved "of"
                    cs <- many1 caseEntry
                    return (CaseExpr s cs)

      -- Parses a single pattern-expression pair in a case statement.
      caseEntry = do p <- pattern
                     m_reservedOp "->"
                     e <- expr
                     m_reservedOp ";"
                     return (p, e)

      -- Parses a subexpression, more specifically either a fat bar ([])
      -- application or a regular application.
      subexpr = buildExpressionParser table term

      -- Terms in a subexpression are either variables or expressions in parens.
      term = var <|> m_parens expr

      -- Parses variables.
      var = fmap fromSymbol m_identifier
      
      -- Table of operators for fat bar ([]) and regular applications.
      -- Weirdly, application seemed easiest to express as a binary whitespace
      -- operator.
      table = [ [Infix (m_reserved "[]" >> return FatbarExpr) AssocLeft]
              , [Infix (m_whiteSpace >> return AppExpr) AssocLeft]
              ]
      
      pattern = fmap VarPat m_identifier

-- Record that holds lexical parsers.
-- Parsec builds the token parser for us from a language definition.
-- All that needs to be done is to name the elements we plan on using.
TokenParser { parens     = m_parens
            , identifier = m_identifier
            , reserved   = m_reserved
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace } = makeTokenParser ldef
 
-- Parsec LanguageDef record, which contains all parameterizable features of the
-- Text.Parsec.Token module.
-- This is used to generate the TokenParser.
ldef :: LanguageDef st
ldef = emptyDef { identStart      = alphaNum <|> oneOf "+-*/="
                , identLetter     = alphaNum <|> oneOf "+-*/="
                , reservedNames   = ["let", "letrec", "in", "case", "of"]
                , reservedOpNames = ["\\", ".", "->", "[]", ",", ";"]
                }
