{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Eval where

import qualified Data.HashMap.Strict as M

import Control.Monad.Error (throwError, runErrorT)
import Control.Monad.State hiding (guard)
import Data.Maybe (fromJust)

import KLambda.Types
import KLambda.Env
import Prelude hiding (exp)

instance KlFun Func where
    apply (Closure env argName body) arg = do
      arg' <- eval arg
      evalWEnv (insertSymEnv argName arg' env) body
    apply (StdFun f) arg = apply f arg

instance KlFun (Kl Val) where
    apply v arg = do
      v' <- v
      case v' of
        VFun f -> apply f arg
        VSym (Symbol s) -> do
          fenv <- gets fst
          apply (fromJust $ M.lookup s fenv) arg
        inv ->
          throwError TypeError{ expectedTy = TyFun, foundTy = typeOf inv }

instance KlVal (Exp -> Kl Val) where klVal = VFun . StdFun

evalKl :: Env -> Exp -> IO (Either KlException Val)
evalKl env exp = runErrorT $ evalStateT (runKl $ eval exp) env

evalWEnv :: Env -> Exp -> Kl Val
evalWEnv env exp = Kl . lift $ evalStateT (runKl $ eval exp) env

eval :: Exp -> Kl Val
eval (ESym s)  = return $ VSym (Symbol s)
eval (EBool b) = return $ VBool b
eval (EStr s)  = return $ VStr s
eval (ENum n)  = return $ VNum n
eval EUnit     = return $ VList []
eval (ELambda param body) = do
    env <- get
    return $ VFun (Closure env param body)
eval (EIf guard then_ else_) = do
    g <- eval guard
    case g of
      VBool True  -> eval then_
      VBool False -> eval else_
      notBool ->
        throwError TypeError{ foundTy = typeOf notBool, expectedTy = TyBool }
eval (EApp e1 e2) = apply (eval e1) e2
