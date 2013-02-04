{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Eval where

import Parser

import qualified Data.HashMap.Strict as M

import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

data Val
    = SymV Symbol
    | BoolV Bool
    | StringV String
    | NumberV Int
    | StreamV
    | ExceptionV KlException
    | VectorV
    | FunctionV
    | ListV
    | TupleV
    | ClosureV
    deriving Show

type Env = M.HashMap Symbol Val

data KlException = KlException String deriving Show

instance Error KlException where
    strMsg = KlException

newtype Kl a = Kl { runKl :: StateT Env (ErrorT KlException IO) a }
    deriving ( Functor, Applicative, Monad, MonadState Env
             , MonadIO, MonadError KlException )

evalKl :: Env -> Kl a -> IO (Either KlException a)
evalKl env k = runErrorT $ evalStateT (runKl k) env

eval :: Sexp -> Kl Val
eval (Sym symbol) = liftM (fromJust . M.lookup symbol) get
eval (Bool True)  = return $ BoolV True
eval (Bool False) = return $ BoolV False
eval (String s)   = return $ StringV s
eval (Number n)   = return $ NumberV (read n)


-- Boolean operations --------------------------------------
eval (App (Sym "if") [guard, thenE, elseE]) = do
    BoolV guardV <- eval guard
    if guardV
      then eval thenE
      else eval elseE

eval (App (Sym "and") [arg1, arg2]) = do
    BoolV arg1' <- eval arg1
    if arg1'
      then eval arg2
      else return $ BoolV False

eval (App (Sym "or") [arg1, arg2]) = do
    BoolV arg1' <- eval arg1
    if arg1'
      then return $ BoolV True
      else eval arg2

eval (App (Sym "cond") cases) = evalCases cases
  where evalCases [] = return $ SymV "nil"
        evalCases ((App guard [body]):rest) = do
          BoolV guardv <- eval guard
          if guardv
            then eval body
            else evalCases rest

-- Symbols -------------------------------------------------
eval (App (Sym "intern") [s]) = do
    StringV s' <- eval s
    return $ SymV s'

-- Strings -------------------------------------------------
eval (App (Sym "pos") [string, idx]) = do
    StringV stringv <- eval string
    NumberV idxv    <- eval idx
    return $ StringV [stringv !! idxv]

eval (App (Sym "tlstr") [string]) = do
    StringV stringv <- eval string
    return $ StringV (tail stringv)

eval (App (Sym "cn") [string1, string2]) = do
    StringV strv1 <- eval string1
    StringV strv2 <- eval string2
    return $ StringV (strv1 ++ strv2)

eval (App (Sym "str") [atom]) = liftM (StringV . show) (eval atom)

eval (App (Sym "n->string") [num]) = liftM (StringV . show) (eval num)

eval (App (Sym "string->n") [nums]) = do
    StringV s <- eval nums
    return $ NumberV (read s)

-- Assignments ---------------------------------------------
eval (App (Sym "set") [Sym s, val]) = do
    val' <- eval val
    modify (M.insert s val')
    return val'

eval (App (Sym "value") [Sym s]) = liftM (fromJust . M.lookup s) get

-- Error handling ------------------------------------------
eval (App (Sym "simple-error") [s]) = do
    StringV str <- eval s
    throwError $ KlException str

{-eval (App (Sym "trap-error") [fun, handler]) = do
    r <- eval fun
    case r of
      ExceptionV exc -> do
        handler' <- eval handler

        eval (App handler [ExceptionV exc])
      _ -> return r-}

eval (App (Sym "error-to-string") [ex]) = do
    ExceptionV exc <- eval ex
    return $ StringV (show exc)


-- Lists ---------------------------------------------------


