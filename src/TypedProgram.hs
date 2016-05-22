module TypedProgram where
import EnrichedLambda
import EnrichedLambdaParse
import TypeDef
import Control.Applicative
import Text.Parsec
import Text.Parsec.String

data TypedProgram = TypedProgram [TypeDef] (Expr TypeDef) deriving (Eq, Show)
data TypedProgramError = E1 ConstructorUndefinedError
                       | E2 ParseError
                         deriving (Show)

parseTypedProgram :: String
                  -> Either TypedProgramError TypedProgram
parseTypedProgram s = case parse typedProgramParser "" s of
                        (Right (Left cuError)) -> Left (E1 cuError)
                        (Left parseError)      -> Left (E2 parseError)
                        (Right (Right tp))     -> Right tp
        
typedProgramParser :: Parser (Either ConstructorUndefinedError TypedProgram)
typedProgramParser = do tds <- typeDefParser
                        e <- exprParser
                        let constrLookup = getConstructorType tds
                        return (TypedProgram tds <$> fillInTypeInfo constrLookup e)
                                                   
