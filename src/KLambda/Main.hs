{-# OPTIONS_GHC -Wall #-}
module KLambda.Main where

import qualified Data.HashMap.Strict as M

import KLambda.Types
import KLambda.Eval
import KLambda.Lexer
import KLambda.Parser
import Text.Parsec (parse)
import Control.Monad.State
import Control.Monad.Error
import qualified KLambda.Fun as F

import Prelude hiding (exp)

stdenv :: Env
stdenv = (F.stdenv, M.empty)

readAndEval :: Kl ()
readAndEval = do
    liftIO $ putStr "> "
    input <- liftIO getLine
    case parse exp "klambda" (alexScanTokens' input) of
      Left err -> error (show err)
      Right exp' -> do
        val <- eval exp'
        liftIO $ putStrLn $ show val
        readAndEval

main :: IO ()
main = do
    r <- runErrorT $ evalStateT (runKl readAndEval) stdenv
    putStrLn $ show r
