{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Eval where

import Control.Monad.Error (throwError, catchError)
import Control.Monad.State hiding (guard)

import KLambda.Types
import KLambda.Env
import KLambda.Unparse
import Prelude hiding (exp, lookup)

instance KlFun Func where
    apply (Closure env (Symbol argName) body) arg =
      eval (insert argName arg env) body
    apply (StdFun f) arg = apply f arg

eval :: LexEnv -> Exp -> Kl Val
eval env (ESym s) =
    return $ case lookup s env of
               Nothing -> VSym (Symbol s)
               Just v  -> v
eval _ (EBool b) = return $ VBool b
eval _ (EStr s)  = return $ VStr s
eval _ (ENum n)  = return $ VNum n
eval _ EUnit     = return $ VList []
eval env (ELambda param body) =
    return $ VFun (Closure env param body)
eval env (EIf guard then_ else_) = do
    g <- eval env guard
    case g of
      VBool True  -> eval env then_
      VBool False -> eval env else_
      notBool ->
        throwError TypeError{ foundTy = typeOf notBool, expectedTy = TyBool }
eval env (EApp e1 e2) = do
    v1 <- eval env e1
    case v1 of
      VSym s -> do
        f' <- lookupFun' s
        case f' of
          Nothing ->
            case s of
              Symbol "eval-kl" -> do
                v2 <- eval env e2
                case unparse [v2] of
                  Left _ -> error "parse error"
                  Right e' -> eval env e'

              Symbol "freeze" -> return $ VCont e2

              Symbol "trap-error" ->
                let handler :: KlException -> Kl Val
                    handler (UserError err) = return $ VFun . StdFun $ \userHandler -> do
                      (handlerFun :: Func) <- ensureType userHandler
                      apply handlerFun (VErr err)
                    handler err             = throwError err
                 in eval env e2 `catchError` handler

              _ -> error $ "undefined symbol: " ++ show s

          Just f -> apply f =<< eval env e2

      VFun f -> apply f =<< eval env e2

      notFun -> throwError TypeError{ foundTy = typeOf notFun, expectedTy = TyFun }

eval env (EDefun (Symbol name) lambda) = do
    VFun c@Closure{} <- eval env lambda
    modify $ \e -> insertFunEnv (Symbol name) c e
    return $ VFun c
