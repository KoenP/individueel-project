module TypedLambdaParse where
import qualified TypedLambda as TL
import qualified EnrichedLambda as EL
import Constant
import Pattern
import TypeDef (TypeDef, Constructor)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

parseTypedExpr :: String -> Either ParseError (TL.TypedExpr ())
parseTypedExpr = parse typedExprParser ""

unsafeParseTypedExpr :: String -> (TL.TypedExpr ())
unsafeParseTypedExpr s = let (Right td) = parseTypedExpr s in td

-- Parser for typed lambda calculus.
typedExprParser :: Parser (TL.TypedExpr ())
typedExprParser = m_whiteSpace >> typedExpr <* eof
    where
      typedExpr = TL.TypedExpr <$> many typeDef <*> expr

      expr =     (abstr       <?> "abstraction"       )
             <|> (letExpr     <?> "let expression"    )
             <|> (letrecExpr  <?> "letrec expression" )
             <|> (caseExpr    <?> "case expression"   )
             <|> (try subexpr <?> "subexpression"     )
             <|> (constExpr   <?> "constant"          )
             <|> (varExpr     <?> "variable"          )

      -- Parses abstractions.
      -- The internal representation only provides single-argument abstractions,
      -- but the notation allows multiple-argument abstractions.
      -- This parser parses multiple-argument abstractions into nested single-
      -- argument abstractions.
      abstr = do m_reservedOp "\\"
                 ps <- many1 pattern
                 m_whiteSpace >> char '.' >> m_whiteSpace
                 body <- expr
                 return $ foldr ((.) . EL.AbstrExpr) id ps body

      -- Parses a simple let expression.
      -- A let expression with multiple comma-separated definitions is 
      -- translated to a nesting of single-definition let expressions.
      letExpr = do m_reserved "let"
                   ds <- sepBy1 definition (m_reservedOp ",")
                   m_reserved "in"
                   e <- expr
                   return $ foldr ((.) . EL.LetExpr) id ds e
                          
      -- Parses a recursive let expression.
      -- There is a deviation from the book notation here: definitions
      -- are separated by a comma (,).
      letrecExpr = do m_reserved "letrec"
                      ds <- sepBy1 definition (m_reservedOp ",")
                      m_reserved "in"
                      e <- expr
                      return (EL.LetrecExpr ds e)

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
                    s <- expr
                    m_reserved "of"
                    cs <- many1 caseEntry
                    return (EL.CaseExpr s cs)

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
      term = constExpr <|> varExpr <|> m_parens expr

      -- Parses variables.
      varExpr = fmap EL.VarExpr m_identifier

      -- Parses constants.
      constExpr = fmap EL.ConstExpr constant
      
      -- Table of operators for fat bar ([]) and regular applications.
      -- Weirdly, application seemed easiest to express as an infix whitespace
      -- operator.
      table = [ [Infix (m_whiteSpace    >> return EL.AppExpr    ) AssocLeft]
              , [Infix (m_reserved "[]" >> return EL.FatbarExpr ) AssocLeft]
              ]
      
      -- Parse a pattern - either a variable or a constructor followed by a
      -- sequence of patterns.
      pattern = fmap ConstPat constant
                <|> fmap VarPat m_identifier
                <|> m_parens (ConstrPat () <$> m_identifier
                                           <*> many pattern)

      -- Parse a constant, which for the time being, can only be an integer.
      constant = fmap IntConst integer
                 <|> (m_reserved "+"      >> return PlusConst)
                 <|> (m_reserved "-"      >> return MinusConst)
                 <|> (m_reserved "SELECT" >> return SelectConst)
                 <|> (m_reserved "="      >> return EqConst)
                 <|> (m_reserved "IF"     >> return IfConst)
                 <|> (m_reserved "_"      >> return ErrorConst)
                 <|> (m_reserved "*"      >> return MultConst)

      -- Parse an integer (positive or negative whole number).
      -- Negative numbers should be parenthesized.
      integer = (fmap read (many1 digit) <* m_whiteSpace)
                <|> (try $ m_parens $ option id (char '-' >> pure negate) <*> integer)

      -- Parse a type definition.
      typeDef = do m_reserved "data"
                   s <- m_identifier
                   m_reservedOp "="
                   cs <- sepBy1 constructor (m_reservedOp "|")
                   m_reservedOp ";"
                   return (TypeDef s cs)

      -- Parse a constructor.
      constructor = Constructor <$> m_identifier <*> many m_identifier

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
ldef = emptyDef { identStart      = letter <|> oneOf "+-*/="
                , identLetter     = alphaNum <|> oneOf "+-*/="
                , reservedNames   = ["let", "letrec", "in", "case", "of",
                                     "+", "-", "=", "IF", "_", "*", "data"]
                , reservedOpNames = ["\\", ".", "->", "[]", ",", ";", "|", "="]
                , caseSensitive   = True
                }
