{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Types where

import           KLambda.Vector

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified Data.Text.Class     as T
import           Data.Typeable       hiding (typeOf)
import           GHC.Generics        (Generic)
import           System.IO           (Handle)
import           Text.Parsec         (ParseError)

type Number = Double
newtype Symbol = Symbol T.Text deriving (Show, Eq, Ord, Generic)

instance T.ToText Symbol where
    toText (Symbol s) = s

instance Hashable Symbol where
    hashWithSalt i (Symbol s) = hashWithSalt i s

data Exp
    = ESym  T.Text
    | EBool Bool
    | EStr  T.Text
    | ENum  Number
    | EApp  Exp (Maybe Exp)
    | ELambda (Maybe Symbol) Exp
    | EIf Exp Exp Exp -- guard, then case, else case
    | EDefun Symbol Exp
    | EUnit
    deriving (Show, Generic)

instance T.ToText Exp where
    toText = T.pack . show

data Val
    = VSym Symbol
    | VBool Bool
    | VStr T.Text
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

instance T.ToText Val where
    toText (VSym (Symbol s)) = s
    toText (VBool b) = if b then "true" else "false"
    toText (VStr s) = s
    toText (VNum n)
      | (floor n :: Int) == (ceiling n :: Int) = T.toText (floor n :: Int)
      | otherwise = T.toText n
    toText lst@VList{} =
      if null elems
        then "()"
        else let l = last elems
                 f = init elems
                 s = case l of
                       VUnit -> ""
                       _ -> " | " `T.append` T.toText l
              in "(" `T.append` T.unwords (map T.toText f) `T.append` s `T.append` ")"
      where
        elems :: [Val]
        elems = listOfVList lst
    toText VFun{} = "<function>"
    toText VSFun{} = "<special form>"
    toText VVec{} = "<vector>"
    toText VStream{} = "<stream>"
    toText VErr{} = "<error>"
    toText VUnit = "<unit>"

instance Show Val where
    show = T.unpack . T.toText

toStr :: Val -> IO T.Text
toStr (VVec v) = liftIO (vectorToText v)
toStr v = return (T.toText v)

data Type
    = TySym | TyStr | TyNum | TyBool | TyStream | TyExc
    | TyVec | TyFun | TyList | TyTuple | TyClos | TyCont | TyUnit
    deriving (Show, Eq, Generic)

data KlException
    = TypeError     { foundTy :: Type, expectedTy :: Type }
    | ArityMismatch { foundAr :: Int, expectedAr :: Int }
    | KlParseError ParseError
    | KlLexerError String Int Int
    | UserError T.Text
    | UnboundSymbol Symbol
    | SerializationError Val
    | VectorErr VectorException
    | ErrMsg String
    | IOError IOError
    | DynamicLoadError String
    | IndexOutOfRange
    deriving (Show, Typeable)

instance Exception KlException

instance Error KlException where
    strMsg = ErrMsg

type SymEnv = M.HashMap T.Text Val
type FunEnv = M.HashMap T.Text Func
type LexEnv = M.HashMap T.Text Val

type Env = (FunEnv, SymEnv)

newtype Kl a = Kl { runKl :: StateT Env IO a }
    deriving ( Functor, Applicative, Monad, MonadState Env, MonadIO, MonadCatch )

type SFun = LexEnv -> Exp -> Kl Val

class KlFun a where
    apply  :: a -> Maybe Val -> Kl Val
    arity  :: a -> Int
    mkFun1 :: a -> (Val -> Kl Val)

type KlFun0 = Kl Val
type KlFun1 = Val -> KlFun0
type KlFun2 = Val -> KlFun1
type KlFun3 = Val -> KlFun2

data Func = Closure LexEnv (Maybe Symbol) Exp
          | forall f. (KlFun f) => StdFun f

instance KlFun (Kl Val) where
    apply f Nothing  = f
    apply _ Just{}   = throwM ArityMismatch{foundAr=1, expectedAr=0}
    arity _          = 0
    mkFun1 f _       = f

instance KlFun (Val -> Kl Val) where
    apply f Nothing  = return $ VFun . StdFun $ f
    apply f (Just e) = f e
    arity _          = 1
    mkFun1           = id

instance KlFun (Val -> Val -> Kl Val) where
    apply f Nothing  = return $ VFun . StdFun $ f
    apply f (Just e) = return (VFun . StdFun $ f e)
    arity _          = 2
    mkFun1 f val1 = apply f (Just val1)

instance KlFun (Val -> Val -> Val -> Kl Val) where
    apply f Nothing  = return $ VFun . StdFun $ f
    apply f (Just e) = return (VFun . StdFun $ f e)
    arity _          = 3
    mkFun1 f val1    = apply f (Just val1)

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

instance EnsureType T.Text where
    ensureType (VStr s) = return s
    ensureType notStr   =
      throwM TypeError{ foundTy = typeOf notStr, expectedTy = TyStr }

instance EnsureType [Char] where
    ensureType (VStr s) = return $ T.unpack s
    ensureType notStr =
      throwM TypeError{ foundTy = typeOf notStr, expectedTy = TyStr }

instance EnsureType Bool where
    ensureType (VBool b) = return b
    ensureType notBool   =
      throwM TypeError{ foundTy = typeOf notBool, expectedTy = TyBool }

instance EnsureType Double where
    ensureType (VNum n) = return n
    ensureType notNum   =
      throwM TypeError{ foundTy = typeOf notNum, expectedTy = TyNum }

instance EnsureType Int where
    ensureType (VNum n) = return $ floor n
    ensureType notNum   =
      throwM TypeError{ foundTy = typeOf notNum, expectedTy = TyNum }

instance EnsureType Func where
    ensureType (VFun f) = return f
    ensureType notFun   =
      throwM TypeError{ foundTy = typeOf notFun, expectedTy = TyFun }

instance EnsureType Symbol where
    ensureType (VSym s) = return s
    ensureType notSym =
      throwM TypeError{ foundTy = typeOf notSym, expectedTy = TySym }

instance EnsureType (Vector Val) where
    ensureType (VVec v) = return v
    ensureType notVec =
      throwM TypeError{ foundTy = typeOf notVec, expectedTy = TyVec }

instance EnsureType Handle where
    ensureType (VStream v) = return v
    ensureType notStream =
      throwM TypeError{ foundTy = typeOf notStream, expectedTy = TyStream }

instance EnsureType KlException where
    ensureType (VErr err) = return err
    ensureType notErr =
      throwM TypeError{ foundTy = typeOf notErr, expectedTy = TyExc }

class KlVal a where
    klVal :: a -> Val

instance KlVal Val    where klVal = id
instance KlVal Int    where klVal = VNum . fromIntegral
instance KlVal Bool   where klVal = VBool
instance KlVal Char   where klVal = VStr . T.singleton
instance KlVal Double where klVal = VNum
instance KlVal Symbol where klVal = VSym
instance KlVal (Vector Val) where klVal = VVec
instance KlVal Handle where klVal = VStream
