module Impoverish where
import qualified EnrichedLambda as EL
import EnrichedLambda (Pattern(..), Def)
import qualified Lambda as L
import Constant

-- Transform the enriched lambda calculus to ordinary lambda calculus.
impoverish :: EL.Expr -> L.Expr

-- An variable in ELC can be literally translated to a variable in LC.
impoverish (EL.VarExpr v)     = L.Var v

-- An application in ELC can be translated by recursively applying the
-- impoverish algorithm to the operands.
impoverish (EL.AppExpr e f)   = L.App (impoverish e) (impoverish f)


impoverish (EL.AbstrExpr p e)
    = case p
      of (ConstPat k) -> let e' = impoverish e
                             v  = head $ L.nonfreeSymbols e'
                             -- b is IF (= k v) E FAIL
                             b  = L.makeApp (L.Const IfConst)
                                            (L.App (L.App (L.Const EqConst)
                                                          (L.Const k))
                                                   (L.Var v))
                                            [ e'
                                            , L.Const FailConst
                                            ]
                         in L.Abstr v b

                                
impoverish (EL.LetExpr (VarPat v, b) e)
    = L.App (L.Abstr v (impoverish e)) (impoverish b)
