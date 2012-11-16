\begin{code}
module TcTermDM where

import BasicTypes
import Data.IORef
import TcMonad
import List( (\\) )
import Text.PrettyPrint.HughesPJ

------------------------------------------
--      The top-level wrapper           --
------------------------------------------

typecheck :: Term -> Tc Sigma
typecheck e = do { ty <- inferSigma e
                 ; zonkType ty }

-----------------------------------
--      The expected type       -- 
-----------------------------------

data Expected a = Infer (IORef a) | Check a


------------------------------------------
--      tcRho, and its variants         --
------------------------------------------

checkRho :: Term -> Rho -> Tc ()
-- Invariant: the Rho is always in weak-prenex form
checkRho expr ty = tcRho expr (Check ty)

inferRho :: Term -> Tc Rho
inferRho expr 
  = do { ref <- newTcRef (error "inferRho: empty result")
       ; tcRho expr (Infer ref)
       ; readTcRef ref }

tcRho :: Term -> Expected Rho -> Tc ()
-- Invariant: if the second argument is (Check rho),
-- 	      then rho is in weak-prenex form
tcRho (Lit _) exp_ty
  = instSigma intType exp_ty

tcRho (Var v) exp_ty 
  = do { v_sigma <- lookupVar v 
       ; instSigma v_sigma exp_ty }

tcRho (App fun arg) exp_ty
  = do { fun_ty <- inferRho fun
       ; (arg_ty, res_ty) <- unifyFun fun_ty
       ; checkSigma arg arg_ty
       ; instSigma res_ty exp_ty }

tcRho (Lam var body) (Check exp_ty)
  = do { (var_ty, body_ty) <- unifyFun exp_ty 
       ; extendVarEnv var var_ty (checkRho body body_ty) }

tcRho (Lam var body) (Infer ref)
  = do { var_ty  <- newTyVarTy
       ; body_ty <- extendVarEnv var var_ty (inferRho body)
       ; writeTcRef ref (var_ty --> body_ty) }

tcRho (Let var rhs body) exp_ty
  = do { var_ty <- inferSigma rhs
       ; extendVarEnv var var_ty (tcRho body exp_ty) }

tcRho (Ann body ann_ty) exp_ty
   = do { checkSigma body ann_ty
        ; instSigma ann_ty exp_ty }


------------------------------------------
--      inferSigma and checkSigma
------------------------------------------

inferSigma :: Term -> Tc Sigma
inferSigma e
   = do { exp_ty <- inferRho e
        ; env_tys <- getEnvTypes
	; env_tvs <- getMetaTyVars env_tys
        ; res_tvs <- getMetaTyVars [exp_ty]
        ; let forall_tvs = res_tvs \\ env_tvs
        ; quantify forall_tvs exp_ty }

checkSigma :: Term -> Sigma -> Tc ()
checkSigma expr sigma
  = do { (skol_tvs, rho) <- skolemise sigma
       ; checkRho expr rho
       ; env_tys <- getEnvTypes
       ; esc_tvs <- getFreeTyVars (sigma : env_tys)
       ; let bad_tvs = filter (`elem` esc_tvs) skol_tvs
       ; check (null bad_tvs)
               (text "Type not polymorphic enough") }

------------------------------------------
--      Subsumption checking            --
------------------------------------------

instSigma :: Sigma -> Expected Rho -> Tc ()
-- Invariant: if the second argument is (Check rho),
-- 	      then rho is in weak-prenex form
instSigma t1 (Check t2) = unify t1 t2
instSigma t1 (Infer r)  = do { t1' <- instantiate t1
                             ; writeTcRef r t1' }
\end{code}
