{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Types where

import           KLambda.Vector

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Binary
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           GHC.Generics        (Generic)
import           System.IO           (Handle)
import           System.IO.Error     (IOError)
import           Text.Parsec         (ParseError)

type Number = Double
newtype Symbol = Symbol String deriving (Show, Eq, Ord, Generic)
instance Binary Symbol

symStr :: Symbol -> String
symStr (Symbol s) = s

instance Hashable Symbol where
    hashWithSalt i (Symbol s) = hashWithSalt i s

data Exp
    = ESym  String
    | EBool Bool
    | EStr  String
    | ENum  Number
    | EApp  Exp (Maybe Exp)
    | ELambda (Maybe Symbol) Exp
    | EIf Exp Exp Exp -- guard, then case, else case
    | EDefun Symbol Exp
    | EUnit
    deriving (Show, Generic)
instance Binary Exp

data Val
    = VSym Symbol
    | VBool Bool
    | VStr String
    | VNum Number
    | VList Val Val
    | VFun Func
    | VSFun SFun
    | VVec (Vector Val)
    | VStream Handle
    | VErr KlException
    | VUnit

listOfVList :: Val -> [Val]
listOfVList (VList v1 v2) = v1 : listOfVList v2
listOfVList v             = [v]

instance Show Val where
    show (VSym (Symbol s)) = s
    show (VBool b) = if b then "true" else "false"
    show (VStr s) = show s
    show (VNum n)
      | floor n == ceiling n = show (floor n)
      | otherwise = show n
    show lst@VList{} =
      if null elems
        then "()"
        else let l = last elems
                 f = init elems
                 s = case l of
                       VUnit -> ""
                       _ -> " | " ++ show l
              in "(" ++ unwords (map show f) ++ s ++ ")"
      where
        elems :: [Val]
        elems = listOfVList lst
    show VFun{} = "<function>"
    show VSFun{} = "<special form>"
    show VVec{} = "<vector>"
    show VStream{} = "<stream>"
    show VErr{} = "<error>"
    show VUnit = "<unit>"

toStr :: Val -> IO String
toStr (VVec v) = liftIO (vectorToString v)
toStr v = return (show v)

data Type
    = TySym | TyStr | TyNum | TyBool | TyStream | TyExc
    | TyVec | TyFun | TyList | TyTuple | TyClos | TyCont | TyUnit
    deriving (Show, Eq, Generic)
instance Binary Type

data KlException
    = TypeError     { foundTy :: Type, expectedTy :: Type }
    | ArityMismatch { foundAr :: Int, expectedAr :: Int }
    | KlParseError ParseError
    | KlLexerError String Int Int
    | UserError String
    | UnboundSymbol Symbol
    | SerializationError Val
    | VectorErr VectorException
    | ErrMsg String
    | IOError IOError
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
    apply :: a -> Maybe Val -> Kl Val
    arity :: a -> Int

type KlFun0 = Kl Val
type KlFun1 = Val -> KlFun0
type KlFun2 = Val -> KlFun1
type KlFun3 = Val -> KlFun2

data Func = Closure LexEnv (Maybe Symbol) Exp
          | forall f. (KlFun f) => StdFun f

instance KlFun (Kl Val) where
    apply f Nothing = f
    apply _ Just{}  = throwError ArityMismatch{foundAr=1, expectedAr=0}
    arity _ = 0
instance KlFun (Val -> Kl Val) where
    apply f Nothing = return $ VFun . StdFun $ f
    apply f (Just e) = f e
    arity _ = 1
instance KlFun (Val -> Val -> Kl Val) where
    apply f Nothing = return $ VFun . StdFun $ f
    apply f (Just e) = return (VFun . StdFun $ f e)
    arity _ = 2
instance KlFun (Val -> Val -> Val -> Kl Val) where
    apply f Nothing = return $ VFun . StdFun $ f
    apply f (Just e) = return (VFun . StdFun $ f e)
    arity _ = 3

--instance Show Func where
    --show (Closure env arg body) = "Closure{" ++ show env ++ "," ++ show arg ++ "," ++ show body ++ "}"
    --show _ = "func"
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

instance EnsureType (Vector Val) where
    ensureType (VVec v) = return v
    ensureType notVec =
      throwError TypeError{ foundTy = typeOf notVec, expectedTy = TyVec }

instance EnsureType Handle where
    ensureType (VStream v) = return v
    ensureType notStream =
      throwError TypeError{ foundTy = typeOf notStream, expectedTy = TyStream }

instance EnsureType KlException where
    ensureType (VErr err) = return err
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
instance KlVal (Vector Val) where klVal = VVec
instance KlVal Handle where klVal = VStream
