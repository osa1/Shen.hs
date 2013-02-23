{-# LANGUAGE MultiParamTypeClasses #-}
module KLambda.Types where

import Control.Monad.Error

type Number = Double
type Symbol = String

data Type
    = TySymbol | TyString | TyNumber | TyBool | TyStream | TyException
    | TyVector | TyFunction | TyList | TyTuple | TyClosure | TyContinuation
    deriving Show

data KLException
    = TypeError { found :: Type, expected :: Type }
    | ErrMsg String

instance Error KLException where
    strMsg = ErrMsg

data Value
    = VSym String
    | VBool Bool
    | VStr String
    | VNum Number
    deriving Show

data Exp
    = ESym  String
    | EBool Bool
    | EStr  String
    | ENum  Number

    | EApp Exp [Exp]
    | ELambda Symbol Exp
    | EUnit
    deriving Show
