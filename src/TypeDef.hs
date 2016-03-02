module TypeDef where

-- Implements a simple type definition language, to allow better testing of
-- the enriched lambda calculus.

import Symbol
import Pattern
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.Map as M

data TypeDef = TypeDef { getTypeName         :: Symbol
                       , getTypeConstructors :: [Constructor]
                       } deriving (Show, Eq)

data Constructor = Constructor { getConstrName   :: Symbol
                               , getConstrFields :: [Symbol]
                               } deriving (Show, Eq)
data ProductOrSumType = SumType | ProductType deriving (Show, Eq)

getConstructorArity :: [TypeDef] -> Symbol -> Maybe Int
getConstructorArity tds = (`M.lookup` table)
    where
      table        = M.fromList (zip constrNames arities)
      constrNames  = map getConstrName constructors
      arities      = map (length . getConstrFields) constructors
      constructors = getAllConstructors tds

getConstructorType :: [TypeDef] -> Symbol -> Maybe Symbol
getConstructorType tds = (`M.lookup` table)
    where
      table = M.fromList
              $ concat
              $ zipWith (\cns tn -> (,) <$> cns <*> [tn]) constrNamesPerType typeNames
      typeNames = map getTypeName tds
      constrNamesPerType = map (map getConstrName . getTypeConstructors) tds

getAllConstructors :: [TypeDef] -> [Constructor]
getAllConstructors = concatMap getTypeConstructors
                      
sumOrProductType :: [TypeDef] -> Symbol -> Maybe ProductOrSumType
sumOrProductType tds s = fmap (\td -> if (length $ getTypeConstructors td) > 1
                                      then SumType
                                      else ProductType)
                              (M.lookup s table)
    where table = M.fromList $ zip (map getTypeName tds) tds
                      

--------------------------------------------------------------------------------
-- PARSER ----------------------------------------------------------------------
--------------------------------------------------------------------------------
parseTypeDef :: String -> Either ParseError [TypeDef]
parseTypeDef = parse typeDefParser ""

unsafeParseTypeDef :: String -> [TypeDef]
unsafeParseTypeDef s = let (Right td) = parseTypeDef s in td

-- Parser for type definitions.
typeDefParser :: Parser [TypeDef]
typeDefParser = m_whiteSpace >> many def <* eof
    where
      def = do m_reserved "data"
               s <- m_identifier
               m_reservedOp "="
               cs <- sepBy1 constructor (m_reservedOp "|")
               m_reservedOp ";"
               return (TypeDef s cs)

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
                , reservedNames   = ["data"]
                , reservedOpNames = ["|", "=", ";"]
                , caseSensitive   = True
                }
