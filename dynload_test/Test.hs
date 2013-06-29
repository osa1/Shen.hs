{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad       (liftM)
import           Data.Maybe          (fromJust)
import           System.Plugins.Load

import           KLambda.Types

main :: IO ()
main = do
    ret <- load_ "Debug.o" [] "parseVal"
    case ret of
      LoadFailure msg -> print msg
      LoadSuccess _ (f :: KlFun1) -> putStrLn "ok"
