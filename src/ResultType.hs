module ResultType where

data ResultType = IntResult | ListResult ResultType

resultTypeToString :: ResultType -> String
resultTypeToString IntResult       = "i"
resultTypeToString (ListResult rt) = 'l' : resultTypeToString rt
