{-# OPTIONS_GHC -Wall #-}
module KLambda.Test where

import           KLambda.Env         (insertFunEnv)
import           KLambda.Eval
import           KLambda.Main        (loadShen', stdenv)
import           KLambda.Parser      (exp, parseKl)
import           KLambda.Types

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.HashMap.Strict as M
import           Data.Time.Clock     (diffUTCTime, getCurrentTime)
import           Prelude             hiding (exp)

evalWStdEnv :: [String] -> IO [Either KlException Val]
evalWStdEnv = evalList stdenv
  where
    evalList :: Env -> [String] -> IO [Either KlException Val]
    evalList _   [] = return []
    evalList env (e:es) =
      case parseKl exp e e of
        Left err -> do
          rest <- evalList env es
          return (Left err:rest)
        Right e' -> do
          ret <- runErrorT $ runStateT (runKl (eval M.empty e')) env
          case ret of
            Left exception -> do
              rest <- evalList env es
              return (Left exception:rest)
            Right (val, env') -> do
              rest <- evalList env' es
              return (Right val:rest)

evalShen :: FilePath -> [Kl a] -> Kl [a]
evalShen shenPath actions = do
    modify $ insertFunEnv (Symbol "load-shen") (StdFun loadShen')
    _ <- eval M.empty (EApp (ESym "load-shen") (Just (EStr shenPath)))
    sequence actions

timeKl :: Kl a -> Kl a
timeKl action = do
    startTime <- liftIO getCurrentTime
    r <- action
    endTime   <- liftIO getCurrentTime
    liftIO (putStrLn $ "dt: " ++ show (endTime `diffUTCTime` startTime))
    return r

evalShenWStdEnv :: FilePath -> [String] -> IO (Either KlException [Val])
evalShenWStdEnv shenPath exps =
    runErrorT (evalStateT (runKl (evalShen shenPath (map (timeKl . eval M.empty . mkApp) exps))) stdenv)
  where
    mkApp :: String -> Exp
    mkApp str =
      EApp (EApp (ESym "map") (Just (ESym "eval")))
           (Just (EApp (ESym "read-from-string") (Just (EStr str))))

binaryFileTests :: FilePath -> [FilePath] -> IO [String]
binaryFileTests benchmarkFilePath binPaths = do
    bfContents <- readFile benchmarkFilePath
    return $ bfContents : map (\path -> "(let _ (read-file-as-bytelist \"" ++ path ++ "\") ())") binPaths

main :: IO ()
main = do
    strs <- binaryFileTests "/home/omer/Shen/Shen 12/Benchmarks/README.shen" [ "/home/omer/Shen/Shen 12/Benchmarks/plato.jpg", "/home/omer/Shen/Shen 12/Benchmarks/heatwave.gif" ]
    print =<< evalShenWStdEnv "/home/omer/Shen/hs_yeni/K Lambda" strs
