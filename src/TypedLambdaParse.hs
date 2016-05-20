module TypedLambdaParse where
import qualified TypedLambda as TL
import qualified EnrichedLambda as EL
import Constant
import Pattern
import Fix
import TypeDef (TypeDef(..), Constructor(..))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

--parseTypedExpr :: String -> Either ParseError TL.TypedProgram
parseTypedExpr = parse typedExprParser ""
--
--unsafeParseTypedExpr :: String -> TL.TypedProgram
--unsafeParseTypedExpr s = let (Right td) = parseTypedExpr s in td

tlVarExpr = In . Ann Nothing . EL.VarExpr
tlConstExpr = In . Ann Nothing . EL.ConstExpr
tlAbstrExpr x e = In . Ann Nothing $ (EL.AbstrExpr x e)
tlLetExpr x e = In . Ann Nothing $ (EL.LetExpr x e)
tlLetrecExpr x e = In . Ann Nothing $ (EL.LetrecExpr x e)
tlCaseExpr :: TL.TypedExpr typeinfo
           -> [(Pattern typeinfo, TL.TypedExpr typeinfo)]
           -> TL.TypedExpr typeinfo
tlCaseExpr e cs = In . Ann Nothing $ (EL.CaseExpr e cs)

-- Parser for typed lambda calculus.
typedExprParser :: Parser (TL.TypedExpr ())
typedExprParser = m_whiteSpace >> expr <* eof
    where

      expr :: Parser (TL.TypedExpr ())
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
      abstr :: Parser (TL.TypedExpr ())
      abstr = do m_reservedOp "\\"
                 ps <- many1 pattern
                 m_whiteSpace >> char '.' >> m_whiteSpace
                 body <- expr
                 return $ foldr ((.) . tlAbstrExpr) id ps body

      -- Parses a simple let expression.
      -- A let expression with multiple comma-separated definitions is 
      -- translated to a nesting of single-definition let expressions.
      letExpr :: Parser (TL.TypedExpr ())
      letExpr = do m_reserved "let"
                   ds <- sepBy1 definition (m_reservedOp ",")
                   m_reserved "in"
                   e <- expr
                   return $ foldr ((.) . tlLetExpr) id ds e
                          
      -- Parses a recursive let expression.
      -- There is a deviation from the book notation here: definitions
      -- are separated by a comma (,).
      letrecExpr :: Parser (TL.TypedExpr ())
      letrecExpr = do m_reserved "letrec"
                      ds <- sepBy1 definition (m_reservedOp ",")
                      m_reserved "in"
                      e <- expr
                      return (tlLetrecExpr ds e)

      -- Parses a definition. This is used in parsing let
      -- and letrec expressions.
      definition :: Parser (Pattern (), TL.TypedExpr ())
      definition = do p <- pattern
                      m_reservedOp "="
                      e <- expr
                      return (p, e)

      -- Parses a case expression.
      -- Deviation from book notation: all pattern-expression pairs must
      -- be terminated by a semicolon (;).
      caseExpr :: Parser (TL.TypedExpr ())
      caseExpr = do m_reserved "case"
                    s <- expr
                    m_reserved "of"
                    cs <- many1 caseEntry
                    return (tlCaseExpr s cs)

      -- Parses a single pattern-expression pair in a case statement.
      caseEntry :: Parser (Pattern (), TL.TypedExpr ())
      caseEntry = do p <- pattern
                     m_reservedOp "->"
                     e <- expr
                     m_reservedOp ";"
                     return (p, e)

      -- Parses a subexpression, more specifically either a fat bar ([])
      -- application or a regular application.
      subexpr :: Parser (TL.TypedExpr ())
      subexpr = do sub <- buildExpressionParser table term
                   ann <- typeAnnotation
                   return sub
                          -- TODO

      -- Terms in a subexpression are either variables or expressions in parens.
      term :: Parser (TL.TypedExpr ())
      term = constExpr <|> varExpr <|> m_parens expr

      -- Parses variables.
      varExpr :: Parser (TL.TypedExpr ())
      varExpr = fmap tlVarExpr m_identifier

      -- Parses constants.
      constExpr :: Parser (TL.TypedExpr ())
      constExpr = fmap tlConstExpr constant
      
      -- Table of operators for fat bar ([]) and regular applications.
      -- Weirdly, application seemed easiest to express as an infix whitespace
      -- operator.
      table = [ [Infix
                 (m_whiteSpace >> return (\e f -> annotate Nothing $ EL.AppExpr e f))
                 AssocLeft]
              , [Infix
                 (m_reserved "[]"
                   >> return (\e f -> annotate Nothing $ EL.FatbarExpr e f))
                 AssocLeft]
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

      -- Parse a type annotation (always optional).
      typeAnnotation :: Parser TL.TypeAnnotation
      typeAnnotation = option Nothing $ fmap Just (m_reservedOp "::" >> typeExpr)
                          
      -- Parse a type expression.
      typeExpr :: Parser TL.Type
      typeExpr = m_identifier >> return ()

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
                , reservedOpNames = ["\\", ".", "->", "[]", ",", ";", "|", "=", "::"]
                , caseSensitive   = True
                }
