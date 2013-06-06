{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Serialize where

import           KLambda.Types
import           KLambda.Vector

import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad.Error    (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary
import qualified Data.HashMap.Strict    as M
import           GHC.Generics           (Generic)

data SVal
    = SSym Symbol
    | SBool Bool
    | SStr String
    | SNum Number
    | SList SVal SVal
    | SFunction SFunc
    | SSFun Symbol
    | SVec [SVal]
    | SUnit
    deriving Generic
instance Binary SVal

data SFunc
    = SClosure SLexEnv (Maybe Symbol) Exp
    | SStdFun String
    deriving Generic
instance Binary SFunc

data SLexEnv = SLexEnv [(String, SVal)]
    deriving Generic
instance Binary SLexEnv


class Serializable a b | a -> b where
    serialize :: Binary b => a -> Kl b
    -- TODO: type of deserialize is much more generalized then we really need,
    -- we only need IO operations and ability to raise KlException.
    deserialize :: Binary b => b -> Kl a

instance Serializable Val SVal where
    serialize (VSym s) = return (SSym s)
    serialize (VBool b) = return (SBool b)
    serialize (VStr s) = return (SStr s)
    serialize (VNum n) = return (SNum n)
    serialize (VList v1 v2) = SList <$> serialize v1 <*> serialize v2
    serialize (VFun f) = SFunction <$> serialize f
    serialize (VSFun sf) = SSFun <$> serialize sf
    serialize (VVec vec) = SVec <$> serialize vec
    serialize v@VStream{} = throwError (SerializationError v)
    serialize v@VErr{} = throwError (SerializationError v)
    serialize VUnit = return SUnit

    deserialize (SSym s) = return (VSym s)
    deserialize (SBool b) = return (VBool b)
    deserialize (SStr s) = return (VStr s)
    deserialize (SNum n) = return (VNum n)
    deserialize (SList s1 s2) = VList <$> deserialize s1 <*> deserialize s2
    deserialize (SFunction f) = VFun <$> deserialize f
    deserialize SSFun{} = undefined
    deserialize (SVec vals) = VVec <$> deserialize vals
    deserialize SUnit = return VUnit

instance Serializable Func SFunc where
    serialize (Closure env arg body) = SClosure <$> serialize env <*> return arg <*> return body
    serialize StdFun{} = undefined
    deserialize = undefined

instance Serializable SFun Symbol where
    serialize = undefined
    deserialize = undefined

instance Serializable (Vector Val) [SVal] where
    serialize vec = mapM serialize =<< liftIO (vectorToList vec)
    deserialize lst = liftIO . listToVector =<< mapM deserialize lst

instance Serializable LexEnv SLexEnv where
    serialize env = SLexEnv <$> mapM (\(k, v) -> (,) k <$> serialize v) (M.toList env)
    deserialize (SLexEnv alst) = M.fromList <$> mapM (\(k, v) -> (,) k <$> deserialize v) alst
