module TypeChecker where
import Symbol
import TypeDef
import EnrichedLambda

arrow :: Type -> Type -> Type
arrow = FunctionType

int :: Type
int = DataType "INT" []

cross :: Type -> Type -> Type
cross t1 t2 = DataType "TUP-2" [t1, t2]

list :: Type -> Type
list t = DataType "LIST" [t]

tvars_in :: Type -> Symbol
tvars_in t = tvars_in' t []
    where tvars_in' (DataType x pms) l = foldr tvars_in' l pms
          tvars_in' 
