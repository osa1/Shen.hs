{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module KLambda.Types where

import qualified Data.HashMap.Strict as M

import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

type Number = Double
type Symbol = String

data Type
    = TySym | TyStr | TyNum | TyBool | TyStream | TyExc
    | TyVec | TyFun | TyList | TyTuple | TyClos | TyCont
    deriving (Show, Eq)

data KlException
    = TypeError     { foundTy :: Type, expectedTy :: Type }
    | ArityMismatch { foundAr :: Int, expectedAr :: Int }
    | ErrMsg String

instance Error KlException where
    strMsg = ErrMsg

type SymbolEnv = M.HashMap Symbol Val

type Env = (SymbolEnv, SymbolEnv) -- fun env, symbol env
funEnv, symEnv :: Env -> M.HashMap Symbol Val
funEnv = fst
symEnv = snd

insertSymEnv :: Symbol -> Val -> Env -> Env
insertSymEnv s v (fe, se) = (fe, M.insert s v se)

insertFunEnv :: Symbol -> Val -> Env -> Env
insertFunEnv s f (fe, se) = (M.insert s f fe, se)

data Val
    = VSym String
    | VBool Bool
    | VStr String
    | VNum Number
    | VList [Val]
    | VFun Func
    deriving Show

class KlVal a where
    klVal :: a -> Val

instance KlVal Val    where klVal = id
instance KlVal Int    where klVal = VNum . fromIntegral
instance KlVal Bool   where klVal = VBool
instance KlVal Char   where klVal = VStr . (:[])
instance KlVal Double where klVal = VNum
instance KlVal a => KlVal [a]
    where klVal a = VList $ map klVal a
instance KlVal ([Exp] -> Kl Val)
    where klVal = VFun . StdFun

newtype Kl a = Kl { runKl :: StateT Env (ErrorT KlException IO) a }
    deriving ( Functor, Applicative, Monad, MonadState Env
             , MonadIO, MonadError KlException )

data Func = Closure Env Symbol Exp
          | StdFun ([Exp] -> Kl Val)

instance Show Func where show _ = "func"

data Exp
    = ESym  String
    | EBool Bool
    | EStr  String
    | ENum  Number

    | EApp Exp [Exp]
    | ELambda Symbol Exp
    | EUnit
    deriving Show

typeOf :: Val -> Type
typeOf VSym{}  = TySym
typeOf VBool{} = TyBool
typeOf VStr{}  = TyStr
typeOf VNum{}  = TyNum
typeOf VList{} = TyList
typeOf VFun{}  = TyClos

ensureType :: Val -> Type -> Kl Val
ensureType val ty
  | typeOf val == ty = return val
  | otherwise  = throwError $ TypeError { foundTy = typeOf val, expectedTy = ty }

ensureArity :: Int -> [a] -> Kl [a]
ensureArity n l
  | n == length l = return l
  | otherwise     = throwError $ ArityMismatch { foundAr = length l, expectedAr = n }
