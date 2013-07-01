{-# OPTIONS_GHC -Wall #-}
module KLambda.Env where

import           KLambda.Types

import           Control.Monad       (liftM)
import           Control.Monad.State (get)
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

funEnv :: Env -> FunEnv
funEnv = fst

symEnv :: Env -> SymEnv
symEnv = snd

insertSymEnv :: Symbol -> Val -> Env -> Env
insertSymEnv (Symbol s) v (fe, se) = (fe, M.insert s v se)

insertFunEnv :: Symbol -> Func -> Env -> Env
insertFunEnv (Symbol s) f (fe, se) = (M.insert s f fe, se)

lookupSym :: Symbol -> Env -> Maybe Val
lookupSym (Symbol s) (_, senv) = M.lookup s senv

lookupFun :: Symbol -> Env -> Maybe Func
lookupFun (Symbol s) (fenv, _) = M.lookup s fenv

lookupSym' :: Symbol -> Kl (Maybe Val)
lookupSym' s = liftM (lookupSym s) get

lookupFun' :: Symbol -> Kl (Maybe Func)
lookupFun' s = liftM (lookupFun s) get

insert :: T.Text -> Val -> LexEnv -> LexEnv
insert = M.insert

lookup :: T.Text -> LexEnv -> Maybe Val
lookup = M.lookup
