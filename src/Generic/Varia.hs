module Generic.Varia where
import Data.Either
    
showEitherResult :: Show b => Either String b -> String
showEitherResult = either id show

showError :: Show a => Either a b -> Either String b
showError = mapLeft show
    
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
