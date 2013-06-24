{-# OPTIONS_GHC -Wall #-}
module KLambda.Main where

import           KLambda.Env         (insertFunEnv)
import           KLambda.Eval
import qualified KLambda.Fun         as F
import           KLambda.Parser
import           KLambda.Types

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.HashMap.Strict as M
import           Data.List           (intercalate)
import qualified Data.Set            as S
import           Prelude             hiding (exp)
import           System.Directory    (getDirectoryContents)
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           System.IO           (hFlush, stdin, stdout)

stdenv :: Env
stdenv = (F.stdenv, M.fromList stdvars)
  where
    stdvars = [ ("*language*", VStr "Haskell")
              , ("*implementation*", VStr "GHC")
              , ("*release*", undefined)
              , ("*port*", VStr "0.1")
              , ("*porters*", VStr "Ömer Sinan Ağacan")
              , ("*stinput*", VStream stdin)
              , ("*stoutput*", VStream stdout)
              , ("*home-directory*", VStr "~/")
              , ("*version*", VStr "TODO")
              , ("*maximum-point-sequence-size*", VNum 100)
              , ("*printer*", VUnit)
              , ("*macros*", VUnit)
              ]

readAndEval :: Kl ()
readAndEval = do
    liftIO $ do
      putStr "> "
      hFlush stdout
    input <- liftIO getLine
    case parseKl exps "klambda" input of
      Left err -> do
        liftIO $ print err
        readAndEval
      Right exps' -> do
        vals <- liftM Just (mapM (eval M.empty) exps') `catchError` handler
        case vals of
          Nothing    -> return ()
          Just vals' -> liftIO (putStrLn =<< toStr (last vals'))
        readAndEval
  where
    handler :: KlException -> Kl (Maybe [Val])
    handler err = liftIO (print err) >> return Nothing

evalFiles :: [FilePath] -> Kl ()
evalFiles [] = return ()
evalFiles (path:paths) = do
    liftIO $ do
      putStr ("loading file: " ++ path ++ " ...")
      hFlush stdout
    file <- liftIO $ readFile path
    case parseKl exps path file of
      Left err    -> liftIO $ print err
      Right exps' -> do
        forM_ exps' $ eval M.empty
        liftIO $ putStrLn "loaded."
    evalFiles paths

loadShen :: KlFun1
loadShen path = do
    path' <- ensureType path
    dirContents <- liftIO $ getDirectoryContents path'
    let ss = S.fromList shenSources
        fs = S.fromList dirContents
    if ss `S.isSubsetOf` fs
      then evalFiles (map (path' </>) shenSources) >> eval M.empty (EApp (ESym "shen.shen") Nothing)
      else throwError $ UserError $ concat
             [ "cannot load Shen, this KLambda sources are missing in "
             , path' , ": ", intercalate ", " (S.toList (ss `S.difference` fs)) ]
  where
    shenSources = [ "toplevel.kl", "core.kl", "sys.kl", "sequent.kl"
                  , "yacc.kl", "reader.kl", "prolog.kl", "track.kl"
                  , "load.kl", "writer.kl", "macros.kl", "declarations.kl"
                  , "t-star.kl", "types.kl"
                  ]

main :: IO ()
main = do
    args <- getArgs
    -- TODO: this is a temporary solution, `loadShen` should be in `Fun.hs`
    -- and it should be added to standard environment. Solution to this may
    -- be harder than expected because of circular import problems.
    let env = insertFunEnv (Symbol "load-shen") (StdFun loadShen) stdenv
    case args of
      ["--shen", path] -> do
        r <- runErrorT $ evalStateT (runKl $ eval M.empty (EApp (ESym "load-shen") (Just (EStr path)))) env
        print r
      files -> do
        r <- runErrorT $ evalStateT (runKl $ evalFiles files >> readAndEval) env
        print r
