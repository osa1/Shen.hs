{-# OPTIONS_GHC -Wall #-}
module KLambda.Eval where

import qualified Data.HashMap.Strict as M

import Control.Monad.Error (throwError, runErrorT)
import Control.Monad.State hiding (guard)

import KLambda.Types
import Prelude hiding (exp)

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
