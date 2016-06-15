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

data Type = FunctionType { getFunctionTypeIn  :: Type
                         , getFunctionTypeOut :: Type
                         }
          | DataType     { getDataTypeName       :: Symbol
                         , getDataTypeParameters :: [Type]
                         } 
          | TypeVar      { getTypeVarSymbol :: Symbol
                         }
            deriving (Show, Eq)

data TypeDef = TypeDef { getTypeDefName         :: Symbol
                       , getTypeDefParams       :: [Symbol]
                       , getTypeDefConstructors :: [Constructor]
                       } deriving (Show, Eq)

data Constructor = Constructor { getConstrName   :: Symbol
                               , getConstrFields :: [Type]
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
      constrNamesPerType = map (map getConstrName . getTypeDefConstructors) tds

-- Check whether the type definition defines a product type or a sum type.
-- A sum type is any type with more than one constructor.
-- A product type has exactly one constructor.
-- A type definition without constructors is invalid.
productOrSumType :: TypeDef -> ProductOrSumType
productOrSumType (TypeDef _ _ [_])   = ProductType
productOrSumType (TypeDef _  _ (_:_:_)) = SumType 

-- The constructors of a type are tagged so they can be distinguished at
-- runtime. This function returns this tag, given a type definition and
-- a symbol for a constructor of that type.
getStructureTag :: TypeDef -> Symbol -> Maybe Int
getStructureTag (TypeDef _ _ cs) s = findIndex ((==s) . getConstrName) cs

-- Gets the arity (= number of fields) of a constructor, given its type
-- definition and symbol.
getConstructorArity :: TypeDef -> Symbol -> Maybe Int
getConstructorArity (TypeDef _ _ cs) s
    = length . getConstrFields <$> find ((==s) . getConstrName) cs

-- Create an n-tuple type with a single product constructor.
tupleType :: Int -> TypeDef
tupleType n = TypeDef typeSymbol params [constructor]
    where typeSymbol = ("tup-" ++ show n)
          constructor = Constructor ("TUP-" ++ show n) paramTypes
          paramTypes = map (\s -> DataType s []) params
          params = take n symbols
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
    --where
typeDef = do m_reserved "data"
             name    <- m_identifier
             params  <- manyTill m_identifier (m_reservedOp "=")
             constrs <- sepBy1 constructorParser (m_reservedOp "|")
             m_reservedOp ";"
             return (TypeDef name params constrs)

type' = try functionTypes <|> dataType
dataType = DataType <$> m_identifier <*> many (simpleDataType <|> m_parens type')
simpleDataType = DataType <$> m_identifier <*> pure []

functionTypes = do
  ts <- sepBy1 (simpleDataType <|> m_parens type') (m_reservedOp "->")
  return (foldr1 FunctionType ts)
                   --
  --t1 <- simpleDataType <|> m_parens type'
                   --m_reservedOp "->"
                   --t2 <- simpleDataType <|> m_parens type'
                   --return (FunctionType t1 t2)

constructorParser = Constructor <$> m_identifier
                                <*> many (simpleDataType <|> m_parens type')


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
                , reservedOpNames = ["|", "=", ";", "->"]
                , caseSensitive   = True
                }

--------------------------------------------------------------------------------
-- JUNK ------------------------------------------------------------------------
--------------------------------------------------------------------------------


getAllConstructors :: [TypeDef] -> [Constructor]
getAllConstructors = concatMap getTypeDefConstructors
                      
