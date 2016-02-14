module Lambda where
import Control.Monad
import Control.Applicative ((<*>), (<$>), (<*))
import Data.Char
import Data.List
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Generic.Glutton

-- An expression of the lambda calculus.
data Expr = Var Symbol
          | App Expr Expr
          | Abstr Symbol Expr
            deriving (Eq, Show)

type Symbol = String

-- Perform a single toplevel eta reduction.
etaReduce :: Expr -> Expr
etaReduce (Abstr x (App f@(Abstr _ _) (Var y))) 
    | x == y && not (occursFree x f) = App f (Var x)
etaReduce e = e

-- Returns the set of all variables that occur in the expression.
variables :: Expr -> Set Symbol
variables (Var x) = Set.singleton x
variables (App e f) = Set.union (variables e) (variables f)
variables (Abstr x e) = Set.insert x (variables e)

-- Returns the set of all variables that occur free in the expression.
-- A variable x occurs free if it is not contained within an Abstr with symbol x.
freeVariables :: Expr -> Set Symbol
freeVariables (Var x)     = Set.singleton x
freeVariables (App e f)   = Set.union (freeVariables e) (freeVariables f)
freeVariables (Abstr x e) = Set.delete x (freeVariables e)

-- Tests whether the symbol occurs free in the expression. See freeVariables.
occursFree :: Symbol -> Expr -> Bool
occursFree x e = Set.member x (freeVariables e)

-- Systematically generates all symbols composed of lowercase letters.
symbols :: [Symbol]
symbols = map (:"") ['a'..'z'] ++ ( flip (:) <$> symbols <*> ['a'..'z'] )

-- Systematically generates all symbols composed of lowercase letters which do not occur
-- free in the expression.
nonfreeSymbols :: Expr -> [Symbol]
nonfreeSymbols e = filter (`Set.notMember` freeVariables e) symbols

-- E[M/x] implementation (see Implementation of Functional Programming Languages p22).
-- substitute e m x = e[m/x]
substitute :: Expr -> Expr -> Symbol -> Expr
substitute (Var y) m x = if x == y then m else (Var y)
substitute (App e f) m x = App (substitute e m x) (substitute f m x)
substitute (Abstr y e) m x
    = case (x == y, x `occursFree` e, y `occursFree` m)
      of (True, _, _)        -> (Abstr x e)
         (False, True, True) -> let z = head $ nonfreeSymbols (App e m)
                                in Abstr z $ substitute (substitute e (Var z) y) m x
         otherwise           -> Abstr y $ substitute e m x


-- Test whether the expression is in normal form (contains no redexes).
isNormalForm :: Expr -> Bool
isNormalForm (Var _)     = True
isNormalForm (Abstr _ e) = isNormalForm e
isNormalForm (App e f)   = case e of (Abstr _ _) -> False
                                     e           -> isNormalForm e && isNormalForm f

-- Simple implementation of beta reduction.
-- Performs a single beta reduction, if this is possible (normal order).
betaReduce :: Expr -> Expr
betaReduce (Var x) = Var x
betaReduce (Abstr s e) = Abstr s (betaReduce e)
betaReduce (App (Abstr x e) m) = substitute e m x
betaReduce (App e f) = App (betaReduce e) (betaReduce f)

-- Fully reduces the expression with normal order reduction.
toNormalForm :: Expr -> Expr
toNormalForm e = if isNormalForm e then e else toNormalForm (betaReduce e)

-- Tests whether two expressions differ only in variable names, but not in
-- structure or relations between variables.
alphaConvertible :: Expr -> Expr -> Bool
alphaConvertible e f = alphaReduce e == alphaReduce f

-- A special alpha reduction which renames the variables of an expression in a
-- systematic way (although the precise order of the symbols is somewhat
-- arbitrary). Two expressions which are alpha-interconvertible will result in
-- exactly the same expression.
alphaReduce :: Expr -> Expr
alphaReduce e = feed (glutton e Map.empty) symbols
    where
      glutton :: Expr -> Map Symbol Symbol -> Glutton Symbol Expr
      glutton (Var x) m = case Map.lookup x m
                          of Nothing  -> nibbler Var
                             (Just y) -> Satiated (Var y)
      glutton (App e f) m = let freeVarsInSubExprsSet = freeVariables e `Set.intersection` freeVariables f
                                freeVarsInMSet        = Set.fromList $ Map.keys m
                                newFreeVarsList       = Set.toList $ freeVarsInSubExprsSet `Set.difference` freeVarsInMSet
                            in do newSymbols <- replicateM (length newFreeVarsList) (nibbler id)
                                  let m' = m `Map.union` Map.fromList (zip newFreeVarsList newSymbols)
                                  liftM2 App (glutton e m') (glutton f m')
      glutton (Abstr x e) m = do y <- nibbler id
                                 liftM (Abstr y) (glutton e $ Map.insert x y m)
