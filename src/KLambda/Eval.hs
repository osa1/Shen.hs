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

intern :: Val -> Kl Val
intern v = do
    VStr s <- ensureType v TyStr
    return $ VSym s

value :: [Exp] -> Kl Val
value exps = do
    [e1]   <- ensureArity 1 exps
    v1     <- eval e1
    VSym s <- ensureType v1 TySym
    env    <- gets symEnv
    return $ fromJust (M.lookup s env)

set' :: [Exp] -> Kl Val
set' exps = do
    [e1, e2] <- ensureArity 2 exps
    v1       <- eval e1
    VSym s   <- ensureType v1 TySym
    v2       <- eval e2
    modify $ \m -> insertSymEnv s v2 m
    return v2

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
eval (EApp exp args) = do
    f <- eval exp
    case f of
      VSym "value"  -> value args
      VSym "set"    -> set'  args
      VFun l        -> apply l args
      _ -> error "can't apply a non function value"
