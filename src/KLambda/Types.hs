{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Types where

import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Mutable as MV

import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

import System.IO (Handle)

type Number = Double
newtype Symbol = Symbol String deriving (Show, Eq)

data Exp
    = ESym  String
    | EBool Bool
    | EStr  String
    | ENum  Number
    | EApp Exp Exp
    | ELambda Symbol Exp
    | EUnit
    | EIf Exp Exp Exp -- guard, then case, else case
    | EDefun Symbol Exp
    deriving Show

data Val
    = VSym Symbol
    | VBool Bool
    | VStr String
    | VNum Number
    | VList [Val]
    | VFun Func
        -- TODO: Implement Unbox instance and use unboxed vectors
    | VVec (MV.IOVector Val)
    | VStream Handle
    deriving Show

instance Show (MV.IOVector Val) where
    show _ = "Vector" -- TODO:

data Type
    = TySym | TyStr | TyNum | TyBool | TyStream | TyExc
    | TyVec | TyFun | TyList | TyTuple | TyClos | TyCont
    deriving (Show, Eq)

data KlException
    = TypeError     { foundTy :: Type, expectedTy :: Type }
    | ArityMismatch { foundAr :: Int, expectedAr :: Int }
    | ErrMsg String
    deriving Show

instance Error KlException where
    strMsg = ErrMsg

type SymEnv = M.HashMap String Val
type FunEnv = M.HashMap String Func
type LexEnv = M.HashMap String Val

type Env = (FunEnv, SymEnv)

newtype Kl a = Kl { runKl :: StateT Env (ErrorT KlException IO) a }
    deriving ( Functor, Applicative, Monad, MonadState Env
             , MonadIO, MonadError KlException )

class KlFun a where
    apply :: a -> Val -> Kl Val

instance KlFun (Val -> Kl Val) where
    apply f e = f e
instance KlFun (Val -> Val -> Kl Val) where
    apply f e = return (VFun . StdFun $ f e)
instance KlFun (Val -> Val -> Val -> Kl Val) where
    apply f e = return (VFun . StdFun $ f e)

data Func = Closure LexEnv Symbol Exp
          | forall f. (KlFun f) => StdFun f

instance Show Func where show _ = "func"

typeOf :: Val -> Type
typeOf VSym{}  = TySym
typeOf VBool{} = TyBool
typeOf VStr{}  = TyStr
typeOf VNum{}  = TyNum
typeOf VList{} = TyList
typeOf VFun{}  = TyClos
typeOf VVec{}  = TyVec
typeOf VStream{} = TyStream

class EnsureType a where
    ensureType :: Val -> Kl a

instance EnsureType [Char] where
    ensureType (VStr s) = return s
    ensureType notStr   =
      throwError TypeError{ foundTy = typeOf notStr, expectedTy = TyStr }

instance EnsureType Bool where
    ensureType (VBool b) = return b
    ensureType notBool   =
      throwError TypeError{ foundTy = typeOf notBool, expectedTy = TyBool }

instance EnsureType [Val] where
    ensureType (VList l) = return l
    ensureType notList =
      throwError TypeError{ foundTy = typeOf notList, expectedTy = TyList }

instance EnsureType Double where
    ensureType (VNum n) = return n
    ensureType notNum   =
      throwError TypeError{ foundTy = typeOf notNum, expectedTy = TyNum }

instance EnsureType Int where
    ensureType (VNum n) = return $ floor n
    ensureType notNum   =
      throwError TypeError{ foundTy = typeOf notNum, expectedTy = TyNum }

instance EnsureType Func where
    ensureType (VFun f) = return f
    ensureType notFun   =
      throwError TypeError{ foundTy = typeOf notFun, expectedTy = TyFun }

instance EnsureType Symbol where
    ensureType (VSym s) = return s
    ensureType notSym =
      throwError TypeError{ foundTy = typeOf notSym, expectedTy = TySym }

instance EnsureType (MV.IOVector Val) where
    ensureType (VVec v) = return v
    ensureType notVec =
      throwError TypeError{ foundTy = typeOf notVec, expectedTy = TyVec }

instance EnsureType Handle where
    ensureType (VStream v) = return v
    ensureType notStream =
      throwError TypeError{ foundTy = typeOf notStream, expectedTy = TyStream }

class KlVal a where
    klVal :: a -> Val

instance KlVal Val    where klVal = id
instance KlVal Int    where klVal = VNum . fromIntegral
instance KlVal Bool   where klVal = VBool
instance KlVal Char   where klVal = VStr . (:[])
instance KlVal Double where klVal = VNum
instance KlVal Symbol where klVal = VSym
instance KlVal (MV.IOVector Val) where klVal = VVec
instance KlVal a => KlVal [a]  where klVal a = VList $ map klVal a
instance KlVal Handle where klVal = VStream
