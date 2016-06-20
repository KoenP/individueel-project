module TypeChecker where
import Symbol
import Constant
import Pattern
import TypeDef
import EnrichedLambda
import Data.List ((\\), elem, nub)
import Data.Maybe (fromJust)
import Control.Monad (join)

type Subst = Symbol -> Type
data TypeScheme = TypeScheme [Symbol] Type
type TypeEnv = [(Symbol, TypeScheme)]
--type VExpr = Expr TypeDef
--type VDef = Def TypeDef
type NameSupply = [Int]
type VDef = [(Symbol,VExpr)]
data VExpr = Var Symbol
           | App VExpr VExpr
           | Abstr Symbol VExpr
           | Let VDef VExpr
           | Letrec [VDef] VExpr
           deriving (Show, Eq)

simplifyExpr :: TypeEnv -> Expr TypeDef -> (VExpr, TypeEnv)
simplifyExpr te expr
  = case expr of
      (VarExpr x) -> (Var x, te)
      (ConstExpr c) -> let v = show c
                           phi = delta v (constantType c)
                       in (Var v, sub_scheme phi te)


constantType :: Constant -> Type
constantType c = case c of
  (IntConst i)               -> int
  PlusConst                  -> FunctionType int int
  MinusConst                 -> FunctionType int int
  EqConst                    -> FunctionType int int
  MultConst                  -> FunctionType int int

arrow :: Type -> Type -> Type
arrow = FunctionType

int :: Type
int = DataType "INT" []

cross :: Type -> Type -> Type
cross t1 t2 = DataType "TUP-2" [t1, t2]

list :: Type -> Type
list t = DataType "LIST" [t]

tvars_in :: Type -> [Symbol]
tvars_in t = tvars_in' t []
  where tvars_in' (TypeVar x) l = x:l
        tvars_in' (DataType x params) l = foldr tvars_in' l params
        tvars_in' (FunctionType i o) l = concatMap (\t -> tvars_in' t l) [i, o]

sub_type :: Subst -> Type -> Type
sub_type phi (TypeVar tvn) = phi tvn
sub_type phi (DataType name ts) = DataType name (map (sub_type phi) ts)
sub_type phi (FunctionType inT outT) = FunctionType (sub_type phi inT) (sub_type phi outT)
                                       
scomp :: Subst -> Subst -> Subst
scomp sub2 sub1 tvn = sub_type sub2 (sub1 tvn)

id_subst :: Subst
id_subst = TypeVar

-- A substitution that affects a single type variable.
delta :: Symbol -> Type -> Subst
delta tvn t tvn' | (tvn == tvn') = t
                 | otherwise     = TypeVar tvn'


-- Handles the simplest case of the unification algorithm:
--   TypeVar tvn = t
-- The following assumptions are made about the parameters:
-- (1) phi is an idempotent substitution
-- (2) t is a fixed point of phi
-- (3) tvn is unmoved by phi
extend :: Subst -> Symbol -> Type -> Maybe Subst
extend phi tvn t
  -- Equation is a tautology -> no need to change phi
  | (t == (TypeVar tvn))    = Just phi
  -- Cyclic equation -> reject
  | (tvn `elem` tvars_in t) = Nothing
  -- Otherwise -> add a binding (tvn -> t)
  | otherwise               = Just ((delta tvn t) `scomp` phi)

-- UNIFICATION -----------------------------------------------------------------
-- Given an idempotent substitution phi and a pair of type expressions (t1,t2),
-- return Nothing if there is no extension of phi which unifies the types t1 and
-- t2,
-- return (Just psi) otherwise, where psi is an idempotent unifier of (t1, t2)
-- which extends phi. Psi will also be maximally general.
unify :: Subst -> (Type, Type) -> Maybe Subst

-- The simplest case: the lhs is a simple type variable, and the rhs is any
-- type expression.
unify phi ((TypeVar tvn), t)
  -- If there is no substitution yet for the lhs (tvn is unmoved by phi),
  -- construct psi by simply extending phi.
  | (phitvn == TypeVar tvn) = extend phi tvn phit

  -- Otherwise, unify the substitution for the lhs under phi with the
  -- substitution for the rhs under phi
  | otherwise               = unify phi (phitvn, phit)

  where
    phitvn = phi tvn
    phit = sub_type phi t

-- There is no substitution that can unify a data type and a function type or
-- vice versa.
unify phi (DataType _ _, FunctionType _ _) = Nothing
unify phi (FunctionType _ _, DataType _ _) = Nothing

-- Two concrete data types can only be unified if the concrete part is the
-- same. Their parameters may still be variables, and we still have to try
-- to unify those.
unify phi (DataType tcn ts, DataType tcn' ts')
  | (tcn == tcn') = unifyl phi (ts `zip` ts')
  | otherwise     = Nothing

-- For function types, we unify the in types and the out types with
-- eachother.
unify phi (FunctionType inT outT, FunctionType inT' outT')
  = unifyl phi ([inT, outT] `zip` [inT', outT'])

-- If the first variable is a DataType or FunctionType and the second is a
-- TypeVar.
unify phi (t1, TypeVar x) = unify phi (TypeVar x, t1)

--------------------------------------------------------------------------------
-- Unify a list of equations.
unifyl :: Subst -> [(Type, Type)] -> Maybe Subst
unifyl phi eqns = foldr unify' (Just phi) eqns
  where
    -- A version of unify that deals with the possibility that there may be no
    -- valid phi.
    unify' eqn (Just phi) = unify phi eqn
    unify' eqn Nothing    = Nothing

-- Get a list of non-schematic type variables.
unknowns_scheme :: TypeScheme -> [Symbol]
unknowns_scheme (TypeScheme scvs t) = tvars_in t \\ scvs

-- Apply a substitution to a type scheme.
sub_scheme :: Subst -> TypeScheme -> TypeScheme
sub_scheme phi (TypeScheme scvs t)
  = TypeScheme scvs (sub_type (exclude phi scvs) t)
  where
    -- Make sure the substitution only affects the unknowns.
    exclude phi scvs tvn | tvn `elem` scvs = TypeVar tvn
                         | otherwise       = phi tvn

dom :: [(a,b)] -> [a]
dom = map fst

rng :: Eq a => [(a,b)] -> [b]
rng al = map (val al) (dom al)

val :: Eq a => [(a,b)] -> a -> b
val al k = fromJust (lookup k al)

install :: [(a,b)] -> a -> b -> [(a,b)]
install al k v = (k,v):al

-- Extension of unknowns_scheme to work on type environments.
unknowns_te :: TypeEnv -> [Symbol]
unknowns_te gamma = join $ map unknowns_scheme (rng gamma)

-- Extension of sub_scheme to work on type environments.
sub_te :: Subst -> TypeEnv -> TypeEnv
sub_te phi gamma = [ (x, sub_scheme phi st) | (x, st) <- gamma ]

-- Functions for making up new names for type variables.
next_name :: NameSupply -> Symbol
next_name ns = show ns
deplete :: NameSupply -> NameSupply
deplete (n:ns) = n+2:ns
split :: NameSupply -> (NameSupply, NameSupply)
split ns = (0:ns, 1:ns)
name_sequence :: NameSupply -> [Symbol]
name_sequence ns = next_name ns : name_sequence (deplete ns)

--------------------------------------------------------------------------------

tc :: TypeEnv -> NameSupply -> VExpr -> Maybe (Subst, Type)
tc gamma ns (Var x) = tcvar gamma ns x
--tc gamma ns (VarExpr x) = tcvar gamma ns x
--tc gamma ns (AppExpr e1 e2) = tcap gamma ns e1 e2
--tc gamma ns (AbstrExpr (VarPat x) e) = tclambda gamma ns x e
--tc gamma ns (LetExpr def e) = tclet gamma ns def e
--tc gamma ns (LetrecExpr defs e) = tcletrec gamma ns defs e


tcl :: TypeEnv -> NameSupply -> [VExpr]  -> Maybe (Subst, [Type])
tcl gamma ns [] = Just (id_subst, [])
tcl gamma ns (e:es) = tcl1 gamma ns0 es (tc gamma ns1 e)
  where (ns0, ns1) = split ns

tcl1 gamma ns es Nothing = Nothing
tcl1 gamma ns es (Just (phi,t)) = tcl2 phi t (tcl gamma' ns es)
  where gamma' = sub_te phi gamma

tcl2 phi t Nothing = Nothing
tcl2 phi t (Just (psi, ts)) = Just (psi `scomp` phi, (sub_type psi t) : ts)


tcvar :: TypeEnv -> NameSupply -> Symbol -> Maybe (Subst, Type)
tcvar gamma ns x =  Just (id_subst, newinstance ns scheme)
  where
    scheme = val gamma x

newinstance :: NameSupply -> TypeScheme -> Type
newinstance ns (TypeScheme scvs t)
  = sub_type phi t
  where al = scvs `zip` name_sequence ns
        phi = al_to_subst al

al_to_subst :: [(Symbol, Symbol)] -> Subst
al_to_subst al tvn | tvn `elem` dom al = TypeVar (val al tvn)
                   | otherwise         = TypeVar tvn

tcap :: TypeEnv -> NameSupply -> VExpr -> VExpr -> Maybe (Subst, Type)
tcap gamma ns e1 e2 = tcap1 tvn (tcl gamma ns' [e1,e2])
  where tvn = next_name ns
        ns' = deplete ns

tcap1 tvn Nothing = Nothing
tcap1 tvn (Just (phi, [t1,t2])) = tcap2 tvn (unify phi (t1, t2 `arrow` TypeVar tvn))

tcap2 tvn Nothing = Nothing
tcap2 tvn (Just phi) = Just (phi, phi tvn)

tclambda :: TypeEnv -> NameSupply -> Symbol -> VExpr -> Maybe (Subst, Type)
tclambda gamma ns x e = tclambda1 tvn (tc gamma' ns e)
  where ns' = deplete ns
        gamma' = new_bvar (x,tvn) : gamma
        tvn = next_name ns

tclambda1 tvn Nothing = Nothing
tclambda1 tvn (Just (phi,t)) = Just (phi, (phi tvn) `arrow` t)

new_bvar (x,tvn) = (x, TypeScheme [] (TypeVar tvn))

tclet :: TypeEnv -> NameSupply -> VDef -> VExpr -> Maybe (Subst, Type)
tclet gamma ns (VarPat x, e') e = tclet1 gamma ns0 x e (tcl gamma ns1 [e'])
  where (ns0,ns1) = split ns

tclet1 gamma ns x e Nothing = Nothing
tclet1 gamma ns x e (Just (phi,ts)) = tclet2 phi (tc gamma'' ns1 e)
  where gamma''   = add_decls gamma' ns0 [x] ts
        gamma'    = sub_te phi gamma
        (ns0,ns1) = split ns

tclet2 phi Nothing = Nothing
tclet2 phi (Just (phi',t)) = Just (phi' `scomp` phi, t)

add_decls :: TypeEnv -> NameSupply -> [Symbol] -> [Type] -> TypeEnv
add_decls gamma ns xs ts = zip xs schemes ++ gamma
  where schemes  = map (genbar unknowns ns) ts
        unknowns = unknowns_te gamma
        
genbar unknowns ns t = TypeScheme (map snd al) t'
  where al = scvs `zip` (name_sequence ns)
        scvs = nub (tvars_in t) \\ unknowns
        t' = sub_type (al_to_subst al) t


tcletrec :: TypeEnv -> NameSupply -> [VDef] -> VExpr -> Maybe (Subst, Type)
tcletrec gamma ns defs e = tcletrec1 gamma ns0 nbvs e (tcl (nbvs ++ gamma) ns1 es)
  where (xs, es)   = unzip [ (x, e) | (VarPat x, e) <- defs ]
        (ns0, ns') = split ns
        (ns1, ns2) = split ns'
        nbvs       = new_bvars xs ns2

new_bvars xs ns = map new_bvar (xs `zip` (name_sequence ns))

tcletrec1 gamma ns nbvs e Nothing = Nothing
tcletrec1 gamma ns nbvs e (Just (phi, ts))
  = tcletrec2 gamma' ns nbvs' e (unifyl phi (ts `zip` ts'))
  where ts'    = map old_bvar nbvs'
        nbvs'  = sub_te phi nbvs
        gamma' = sub_te phi gamma

old_bvar (x, TypeScheme [] t) = t

tcletrec2 gamma ns nbvs e Nothing = Nothing
tcletrec2 gamma ns nbvs e (Just phi) = tclet2 phi (tc gamma'' ns1 e)
  where ts         = map old_bvar nbvs'
        nbvs'      = sub_te phi nbvs
        gamma'     = sub_te phi gamma
        gamma''    = add_decls gamma' ns0 (map fst nbvs) ts
        (ns0, ns1) = split ns
