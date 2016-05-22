module TypeDef where

-- Implements a simple type definition language, to allow better testing of
-- the enriched lambda calculus.

import Symbol
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.Map as M
import Data.List (find, findIndex)

data TypeDef = TypeDef { getTypeName         :: Symbol
                       , getTypeConstructors :: [Constructor]
                       } deriving (Show, Eq)

data Constructor = Constructor { getConstrName   :: Symbol
                               , getConstrFields :: [Symbol]
                               } deriving (Show, Eq)
data ProductOrSumType = SumType | ProductType deriving (Show, Eq)

data ConstructorUndefinedError = ConstructorUndefinedError Symbol deriving (Show)

-- Get the type definition to which a constructor (lookup by symbol) belongs.
getConstructorType :: [TypeDef] -> Symbol -> Maybe TypeDef
getConstructorType tds = (`M.lookup` table)
    where
      table = M.fromList
              $ concat
              $ zipWith (\cns td -> (,) <$> cns <*> [td]) constrNamesPerType tds
      constrNamesPerType = map (map getConstrName . getTypeConstructors) tds

-- Check whether the type definition defines a product type or a sum type.
-- A sum type is any type with more than one constructor.
-- A product type has exactly one constructor.
-- A type definition without constructors is invalid.
productOrSumType :: TypeDef -> ProductOrSumType
productOrSumType (TypeDef _ [_])   = ProductType
productOrSumType (TypeDef _ (_:_:_)) = SumType 

-- The constructors of a type are tagged so they can be distinguished at
-- runtime. This function returns this tag, given a type definition and
-- a symbol for a constructor of that type.
getStructureTag :: TypeDef -> Symbol -> Maybe Int
getStructureTag (TypeDef _ cs) s = findIndex ((==s) . getConstrName) cs

-- Gets the arity (= number of fields) of a constructor, given its type
-- definition and symbol.
getConstructorArity :: TypeDef -> Symbol -> Maybe Int
getConstructorArity (TypeDef _ cs) s
    = length . getConstrFields <$> find ((==s) . getConstrName) cs

-- Create an n-tuple type with a single product constructor.
tupleType :: Int -> TypeDef
tupleType n = TypeDef symbol [constructor]
    where symbol = ("tup-" ++ show n)
          constructor = Constructor ("TUP-" ++ show n) (take n symbols)
          symbols = map (:"") ['a'..'z'] ++ (flip (:) <$> symbols <*> ['a'..'z'])

--------------------------------------------------------------------------------
-- PARSER ----------------------------------------------------------------------
--------------------------------------------------------------------------------
parseTypeDef :: String -> Either ParseError [TypeDef]
parseTypeDef = parse typeDefParser ""

unsafeParseTypeDef :: String -> [TypeDef]
unsafeParseTypeDef s = let (Right td) = parseTypeDef s in td

-- Parser for type definitions.
typeDefParser :: Parser [TypeDef]
typeDefParser = many typeDef 
    where typeDef = do m_reserved "data"
                       s <- m_identifier
                       m_reservedOp "="
                       cs <- sepBy1 constructorParser (m_reservedOp "|")
                       m_reservedOp ";"
                       return (TypeDef s cs)

constructorParser = Constructor <$> m_identifier <*> many m_identifier

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


--------------------------------------------------------------------------------
-- JUNK ------------------------------------------------------------------------
--------------------------------------------------------------------------------


getAllConstructors :: [TypeDef] -> [Constructor]
getAllConstructors = concatMap getTypeConstructors
                      
