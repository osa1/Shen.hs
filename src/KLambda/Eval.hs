{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module KLambda.Eval where

import           KLambda.Env
import           KLambda.Parser      (exps)
import           KLambda.Types

import           Control.Monad.Error (catchError, throwError)
import           Control.Monad.State hiding (guard)
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (fromMaybe)
import           Prelude             hiding (exp, lookup)
import           Text.Parsec         (parse)

instance KlFun Func where
    apply (Closure env Nothing body) Nothing = eval env body
    apply c@Closure{}                Nothing = return $ VFun c
    apply (Closure _   Nothing _   ) _       = throwError ArityMismatch{foundAr = 1, expectedAr = 0}
    apply (Closure env (Just (Symbol argName)) body) (Just arg) =
      eval (insert argName arg env) body

    apply f@(StdFun f') Nothing
      | arity f /= 0 = return $ VFun f
      | otherwise = apply f' Nothing
    apply (StdFun f) (Just arg) = apply f (Just arg)

    arity (Closure _ Nothing _) = 0
    arity Closure{}             = 1
    arity (StdFun f) = arity f

evalKl :: SFun
evalKl env exp = do
  val <- eval env exp
  case parse exps "klambda" [val] of
    Left err   -> throwError $ KlParseError err
    Right args -> liftM last $ mapM (eval env) args

freeze :: SFun
freeze env exp = return $ VFun $ Closure env Nothing exp

trapError :: SFun
trapError env exp = return $
    VSFun $ \env' exp' -> eval env exp `catchError` handler env' exp'
  where
    handler :: LexEnv -> Exp -> KlException -> Kl Val
    handler env exp err = do
      handlerFun' :: Func <- ensureType =<< eval env exp
      apply handlerFun' (Just (VErr err))

specials :: M.HashMap Symbol SFun
specials = M.fromList
  [ (Symbol "eval-kl", evalKl)
  , (Symbol "freeze", freeze)
  , (Symbol "trap-error", trapError)
  ]

eval :: LexEnv -> Exp -> Kl Val
eval env (ESym s) = return $ fromMaybe (VSym (Symbol s)) (lookup s env)
eval _ (EBool b)  = return $ VBool b
eval _ (EStr s)   = return $ VStr s
eval _ (ENum n)   = return $ VNum n
eval _ EUnit      = return VUnit
eval env (ELambda param body) =
    return $ VFun (Closure env param body)
eval env (EIf guard then_ else_) = do
    g <- ensureType =<< eval env guard
    eval env (if g then then_ else else_)

eval env (EApp exp Nothing) = do
    val <- eval env exp
    case val of
      VSym s -> do
        fun <- lookupFun' s
        case fun of
          Just f  ->
            if arity f == 0
              then apply f Nothing
              else return $ VFun f
          Nothing ->
            case M.lookup s specials of
              Nothing -> throwError $ UnboundSymbol s
              Just _  -> return $ VFun $ Closure env (Just (Symbol "X"))
                                                     (EApp (ESym (symStr s)) (Just (ESym "X")))
      VFun f ->
        if arity f == 0
          then apply f Nothing
          else return $ VFun f
      VSFun s -> return $ VSFun s
      notFun -> throwError TypeError{foundTy = typeOf notFun, expectedTy = TyFun}

eval env (EApp exp (Just arg)) = do
    val <- eval env exp
    case val of
      VSym s -> do
        fun <- lookupFun' s
        case fun of
          Just f  -> apply f . Just =<< eval env arg
          Nothing -> case M.lookup s specials of
                       Nothing -> throwError $ UnboundSymbol s
                       Just sv -> sv env arg
      VFun f -> apply f . Just =<< eval env arg
      VSFun s -> s env arg
      notFun -> throwError TypeError{foundTy = typeOf notFun, expectedTy = TyFun}

-- TODO: throw an error when redefining a primitive
eval env (EDefun (Symbol name) lambda) = do
    VFun c@Closure{} <- eval env lambda
    modify $ \e -> insertFunEnv (Symbol name) c e
    return $ VSym (Symbol name)
