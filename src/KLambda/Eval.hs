{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Eval where

import KLambda.Types
import KLambda.Env
import KLambda.Parser (exps)

import Control.Monad.Error (throwError, catchError)
import Control.Monad.State hiding (guard)
import qualified Data.HashMap.Strict as M

import Text.Parsec (parse)

import Prelude hiding (exp, lookup)

import Debug.Trace

instance KlFun Func where
    apply (Closure env Nothing body) VUnit = eval env body
    apply (Closure _   Nothing _   ) _     = throwError ArityMismatch{foundAr = 1, expectedAr = 0}
    apply c@Closure{} VUnit = return $ VFun c
    apply (Closure env (Just (Symbol argName)) body) arg =
      eval (insert argName arg env) body

    apply f@StdFun{} VUnit
      | arity f /= 0 = return $ VFun f
    apply (StdFun f) arg = apply f arg

    arity (Closure _ Nothing _) = 0
    arity Closure{}             = 1
    arity (StdFun f) = arity f

evalKl :: SFun
evalKl env exp = do
  val <- eval env exp
  case parse exps "klambda" [val] of
    Left err   -> throwError $ KlParseError err
    Right args -> do
      vals <- mapM (eval env) args
      return $ last vals

freeze :: SFun
freeze _ exp = return $ VCont exp

trapError :: SFun
trapError env exp = return $
    VSFun $ \env' exp' -> do
      eval env exp `catchError` handler env' exp'
  where handler :: LexEnv -> Exp -> KlException -> Kl Val
        handler env exp err =
          case err of
            UserError userErr -> do
              handlerFun' :: Func <- ensureType =<< (eval env exp)
              apply handlerFun' (VErr userErr)
            _ -> throwError err

specials :: M.HashMap Symbol SFun
specials = M.fromList
  [ (Symbol "eval-kl", evalKl)
  , (Symbol "freeze", freeze)
  , (Symbol "trap-error", trapError)
  ]

eval :: LexEnv -> Exp -> Kl Val
eval env (ESym s) =
    return $ case lookup s env of
               Nothing -> VSym (Symbol s)
               Just v  -> v
eval _ (EBool b) = return $ VBool b
eval _ (EStr s)  = return $ VStr s
eval _ (ENum n)  = return $ VNum n
eval _ EUnit     = return $ VUnit
eval _ EEmptyLst = return $ VList []
eval env (ELambda param body) =
    return $ VFun (Closure env param body)
eval env (EIf guard then_ else_) = do
    g <- eval env guard
    case g of
      VBool True  -> eval env then_
      VBool False -> eval env else_
      notBool ->
        throwError TypeError{ foundTy = typeOf notBool, expectedTy = TyBool }

eval env (EApp exp arg) = do
    val <- eval env exp
    case val of
      VSym s -> do
        fun <- lookupFun' s
        case fun of
          Just f -> apply f =<< eval env arg
          Nothing -> case M.lookup s specials of
                       Nothing -> throwError $ UnboundSymbol s
                       Just sv -> sv env arg
      VFun f -> apply f =<< eval env arg
      VSFun s -> s env arg
      _ -> error $ "apply a non-function value: " ++ show val

eval env (EDefun (Symbol name) lambda) = do
    VFun c@Closure{} <- eval env lambda
    modify $ \e -> insertFunEnv (Symbol name) c e
    return $ VFun c
