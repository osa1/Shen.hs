{-# OPTIONS_GHC -Wall #-}
module KLambda.Eval where

import qualified Data.HashMap.Strict as M

import Control.Monad.Error
import Control.Monad.State
import Data.Maybe (fromJust)

import KLambda.Types
import Prelude hiding (exp)

evalKl :: Env -> Kl a -> IO (Either KlException a)
evalKl env k = runErrorT $ evalStateT (runKl k) env

evalWEnv :: Env -> Exp -> Kl Val
evalWEnv env exp = Kl . lift $ evalStateT (runKl $ eval exp) env

ensureType' :: Exp -> Type -> Kl Val
ensureType' exp ty = do
    v1 <- eval exp
    let t = typeOf v1
    if t == ty
      then return v1
      else throwError $ TypeError { foundTy = t, expectedTy = ty }

apply :: Func -> [Exp] -> Kl Val
apply f [] = do
    return $ VFun f

apply (Closure env arg body) [a] = do
    av   <- eval a
    let env' = insertSymEnv arg av env
    evalWEnv env' body

apply c (a:as) = do
    f <- apply c [a]
    case f of
      VFun c' -> apply c' as
      _ -> error "can't apply a non function value"

eval :: Exp -> Kl Val
eval (ESym s)  = return $ VSym s
eval (EBool b) = return $ VBool b
eval (EStr s)  = return $ VStr s
eval (ENum n)  = return $ VNum n
eval EUnit     = return $ VList []
eval (ELambda param body) = do
    env <- get
    return $ VFun (Closure env param body)
eval (EApp (ESym s) args) = do
    fenv <- gets funEnv
    case fromJust (M.lookup s fenv) of
      VFun f -> apply f args
      _ -> error "can't apply a non-function value"
eval (EApp exp args) = do
    f <- eval exp
    case f of
      VFun f' -> apply f' args
      _ -> error "can't apply a non function value"
