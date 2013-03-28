{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Types where

import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Mutable as MV
import Data.Hashable

import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

import Text.Parsec (ParseError)

import System.IO (Handle)

type Number = Double
newtype Symbol = Symbol String deriving (Show, Eq, Ord)

instance Hashable Symbol where
    hashWithSalt i (Symbol s) = hashWithSalt i s

data Exp
    = ESym  String
    | EBool Bool
    | EStr  String
    | ENum  Number
    | EApp  Exp Exp
    | ELambda (Maybe Symbol) Exp
    | EIf Exp Exp Exp -- guard, then case, else case
    | EDefun Symbol Exp
    | EEmptyLst
    -- Unit values/expressions are used in two places,
    --  1) Function applications with no parameter
    --  2) Non-exhaustive cond expressions
    | EUnit
    deriving Show

data Val
    = VSym Symbol
    | VBool Bool
    | VStr String
    | VNum Number
    | VList [Val]
    | VFun Func
    | VSFun SFun
        -- TODO: Implement Unbox instance and use unboxed vectors
    | VVec (MV.IOVector Val)
    | VStream Handle
    | VCont Exp
    | VErr UserErrorMsg
    | VUnit

instance Show Val where
    show (VSym (Symbol s)) = s
    show (VBool b) = if b then "true" else "false"
    show (VStr s) = show s
    show (VNum n) = show n
    show (VList vals) = "(" ++ (unwords (map show vals)) ++ ")"
    show VFun{} = "<function>"
    show VSFun{} = "<special form>"
    show VVec{} = "<vector>"
    show VStream{} = "<stream>"
    show VCont{} = "<continuation>"
    show VErr{} = "<error>"
    show VUnit = "<unit>"

data Type
    = TySym | TyStr | TyNum | TyBool | TyStream | TyExc
    | TyVec | TyFun | TyList | TyTuple | TyClos | TyCont | TyUnit
    deriving (Show, Eq)

newtype UserErrorMsg = UserErrorMsg String deriving (Show, Eq)

data KlException
    = TypeError     { foundTy :: Type, expectedTy :: Type }
    | ArityMismatch { foundAr :: Int, expectedAr :: Int }
    | KlParseError ParseError
    | UserError UserErrorMsg
    | UnboundSymbol Symbol
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

type SFun = LexEnv -> Exp -> Kl Val

class KlFun a where
    apply :: a -> Val -> Kl Val
    arity :: a -> Int

instance KlFun (Val -> Kl Val) where
    apply f e = f e
    arity _ = 1
instance KlFun (Val -> Val -> Kl Val) where
    apply f e = return (VFun . StdFun $ f e)
    arity _ = 2
instance KlFun (Val -> Val -> Val -> Kl Val) where
    apply f e = return (VFun . StdFun $ f e)
    arity _ = 3

data Func = Closure LexEnv (Maybe Symbol) Exp
          | forall f. (KlFun f) => StdFun f

instance Show Func where show _ = "func"

typeOf :: Val -> Type
typeOf VSym{}  = TySym
typeOf VBool{} = TyBool
typeOf VStr{}  = TyStr
typeOf VNum{}  = TyNum
typeOf VList{} = TyList
typeOf VFun{}  = TyClos
typeOf VSFun{} = TyClos -- FIXME
typeOf VVec{}  = TyVec
typeOf VStream{} = TyStream
typeOf VCont{} = TyCont
typeOf VErr{}  = TyExc
typeOf VUnit{} = TyUnit

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

instance EnsureType Exp where
    ensureType (VCont e) = return e
    ensureType notCont =
      throwError TypeError{ foundTy = typeOf notCont, expectedTy = TyCont }

instance EnsureType UserErrorMsg where
    ensureType (VErr msg) = return msg
    ensureType notErr =
      throwError TypeError{ foundTy = typeOf notErr, expectedTy = TyExc }

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
