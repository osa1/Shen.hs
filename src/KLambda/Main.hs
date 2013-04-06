{-# OPTIONS_GHC -Wall #-}
module KLambda.Main where

-- KLambda load order
-- ["toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl" "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl" "macros.kl" "declarations.kl"   "types.kl" "t-star.kl"]

import qualified Data.HashMap.Strict as M

import KLambda.Types
import KLambda.Eval
import KLambda.Lexer
import KLambda.Parser
import Text.Parsec (parse)
import Control.Monad.State
import Control.Monad.Error
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stdin)
import qualified KLambda.Fun as F

import Prelude hiding (exp)

stdenv :: Env
stdenv = (F.stdenv, M.fromList stdvars)
  where stdvars = [ ("*language*", VStr "Haskell")
                  , ("*implementation*", VStr "GHC")
                  , ("*release*", undefined)
                  , ("*port*", VStr "0.1")
                  , ("*porters*", VStr "Ömer Sinan Ağacan")
                  , ("*stinput*", VStream stdin)
                  , ("*stoutput*", VStream stdout)
                  , ("*home-directory*", VStr "~/")
                  , ("*version*", VStr "TODO")
                  , ("*maximum-point-sequence-size*", VNum 100)
                  , ("*printer*", undefined)
                  , ("*macros*", undefined)
                  ]

readAndEval :: Kl ()
readAndEval = do
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parse exps "klambda" (alexScanTokens' input) of
      Left err -> do
        liftIO $ print err
        readAndEval
      Right exps' -> do
        vals <- liftM Just (mapM (eval M.empty) exps') `catchError` handler
        case vals of
          Nothing    -> return ()
          Just vals' -> liftIO $ print vals'
        readAndEval
  where handler :: KlException -> Kl (Maybe [Val])
        handler err = liftIO (print err) >> return Nothing

evalFiles :: [FilePath] -> Kl ()
evalFiles [] = readAndEval
evalFiles (path:paths) = do
    file <- liftIO $ readFile path
    case parse exps path (alexScanTokens' file) of
      Left err    -> liftIO $ print err
      Right exps' -> do
        forM_ exps' $ eval M.empty
        liftIO $ putStrLn $ "loaded file: " ++ path
    evalFiles paths

main :: IO ()
main = do
    args <- getArgs
    r <- runErrorT $ evalStateT (runKl $ evalFiles args) stdenv
    print r
