{-# OPTIONS_GHC -Wall #-}
module KLambda.Test where

import           KLambda.Eval
import           KLambda.Lexer       (alexScanTokens')
import           KLambda.Main        (stdenv)
import           KLambda.Parser      (exp)
import           KLambda.Types

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.HashMap.Strict as M
import           Prelude             hiding (exp)
import           Test.HUnit
import           Text.Parsec         (parse)

evalWStdEnv :: [String] -> IO [Either KlException Val]
evalWStdEnv exps =
    evalList stdenv exps
  where
    evalList :: Env -> [String] -> IO [Either KlException Val]
    evalList _   [] = return []
    evalList env (e:es) = do
      case parse exp e (alexScanTokens' e) of
        Left err -> do
          rest <- evalList env es
          return (Left (KlParseError err):rest)
        Right e' -> do
          ret <- runErrorT $ runStateT (runKl (eval M.empty e')) env
          case ret of
            Left exception -> do
              rest <- evalList env es
              return (Left exception:rest)
            Right (val, env') -> do
              rest <- evalList env' es
              return (Right val:rest)
