{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Fun where

import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, gets)
import Control.Monad.Error (throwError)
import Control.Monad (zipWithM, liftM)
import Data.Maybe (fromJust)
import System.IO (IOMode(..), hGetChar, hClose, hPrint, openFile)
import System.IO.Error (tryIOError)

import KLambda.Types
import KLambda.Env

returnKl :: a -> Kl a
returnKl = return

klEnsureType :: Type -> Func
klEnsureType ty = StdFun $ \v -> returnKl . klVal $ typeOf v == ty

-- String operations
-- --------------------------------------------------------

pos, tlstr, cn, str, strp, nToStr, strToN :: Func
pos = StdFun $ \v1 v2 -> do
  s             <- ensureType v1
  (n :: Double) <- ensureType v2
  return $ VStr [s !! floor n]

tlstr = StdFun $ \v -> do
  s <- ensureType v
  return $ VStr (tail s)

cn = StdFun $ \v1 v2 -> do
  s1 <- ensureType v1
  s2 <- ensureType v2
  return $ VStr (s1 ++ s2)

str = StdFun $ \(v :: Val) -> returnKl $ klVal (show v)

strp = klEnsureType TyStr

nToStr = str

strToN = StdFun $ \v -> do
  n <- ensureType v
  return $ VNum (read n)

-- Lists
-- --------------------------------------------------------

cons, hd, tl, consp :: Func
cons = StdFun $ \(v1 :: Val) v2 -> do
  l  <- ensureType v2
  return $ klVal (v1:l)

hd = StdFun $ \v -> do
  (l :: [Val]) <- ensureType v
  return $ head l

tl = StdFun $ \v -> do
  (l :: [Val]) <- ensureType v
  return $ klVal (tail l)

consp = klEnsureType TyList

-- Arithmetic
-- --------------------------------------------------------

mkArithFun :: forall a. KlVal a => (Double -> Double -> a) -> Func
mkArithFun op = StdFun $ \v1 v2 -> do
  n1 <- ensureType v1
  n2 <- ensureType v2
  return $ klVal (n1 `op` n2)

numberp :: Func
numberp = klEnsureType TyNum

-- Assignments
-- --------------------------------------------------------

set', value :: Func
set' = StdFun $ \v1 v2 -> do
  s <- ensureType v1
  modify $ \env -> insertSymEnv s v2 env
  return v2

value = StdFun $ \v -> do
  Symbol s <- ensureType v
  senv     <- gets symEnv
  return $ fromJust (M.lookup s senv)

-- Symbols
-- --------------------------------------------------------

intern :: Func
intern = StdFun $ \v -> do
  s <- ensureType v
  return $ VSym s

-- Vectors
-- --------------------------------------------------------

absvector, vecAssign, vecRead, vectorp :: Func
absvector = StdFun $ \v -> do
  n   <- ensureType v
  vec <- liftIO $ MV.new n
  return $ VVec vec

vecAssign = StdFun $ \vec idx (val :: Val) -> do
  vec' <- ensureType vec
  idx' <- ensureType idx
  -- TODO: can we assume that Shen implementation in KLambda already does
  -- the bound checking? if so we can use `unsafeWrite`.
  liftIO $ MV.write vec' idx' val
  return vec

vecRead = StdFun $ \vec idx -> do
  vec' <- ensureType vec
  idx' <- ensureType idx
  -- TODO: same situation with `vecAssign`. `unsafeRead` can be used.
  liftIO (MV.read vec' idx' :: IO Val)

vectorp = klEnsureType TyVec

-- Error handling
-- --------------------------------------------------------

simpleError, errorToString :: Func

simpleError = StdFun $ \s -> do
  msg <- ensureType s
  throwError $ UserError $ UserErrorMsg msg :: Kl Val

errorToString = StdFun $ \e -> do
  UserErrorMsg err <- ensureType e
  return $ VStr err

-- Streams and I/O
-- --------------------------------------------------------

pr, readByte, open, close :: Func
pr = StdFun $ \s stream -> do
  (s' :: String) <- ensureType s
  handle <- ensureType stream
  liftIO $ hPrint handle s'
  return $ klVal s'

readByte = StdFun $ \stream -> do
  handle <- ensureType stream
  byte <- liftIO $ tryIOError (hGetChar handle)
  return $ case byte of
             Left _  -> VNum (-1)
             Right b -> VNum (fromIntegral $ fromEnum b)

open = StdFun $ \(_ :: Val) path dir -> do
  path' <- ensureType path
  Symbol dir' <- ensureType dir
  mode <- case dir' of
            "in"  -> return ReadMode
            "out" -> return WriteMode
              -- TODO: maybe I should ensure this part and encode it in syntax tree
            wrong -> throwError $ ErrMsg ("invalid open mode: " ++ wrong)
  handle <- liftIO $ openFile path' mode
  return $ klVal handle

close = StdFun $ \stream -> do
  handle <- ensureType stream
  liftIO $ hClose handle
  return $ VList []

-- Streams and I/O
-- --------------------------------------------------------

eq :: Func
eq = StdFun $ \v1 v2 -> liftM VBool $ eq' v1 v2
  where
    eq' :: Val -> Val -> Kl Bool
    eq' (VSym s1)  (VSym s2)  = return $ s1 == s2
    eq' (VBool b1) (VBool b2) = return $ b1 == b2
    eq' (VStr s1)  (VStr s2)  = return $ s1 == s2
    eq' (VNum n1)  (VNum n2)  = return $ n1 == n2
    eq' (VList l1) (VList l2) =
      if length l1 /= length l2
        then return False
        else liftM and $ zipWithM eq' l1 l2
    eq' VFun{}     VFun{}     = return False
    eq' (VVec v1)  (VVec v2)  =
      -- TODO: zipWithM for IOVectors ??
      if MV.length v1 /= MV.length v2
        then return False
        else do
          v1' <- liftIO $ V.freeze v1
          v2' <- liftIO $ V.freeze v2
          liftM V.and $ V.zipWithM eq' v1' v2'
    eq' VStream{}  VStream{}  = return False
    eq' _          _          = return False


-- Standard environment
-- --------------------------------------------------------

stdenv :: M.HashMap String Func
stdenv = M.fromList
  [ ("pos", pos)
  , ("tlstr", tlstr)
  , ("cn", cn)
  , ("str", str)
  , ("string?", strp)
  , ("n->string", nToStr)
  , ("string->n", strToN)
  , ("cons", cons)
  , ("hd", hd)
  , ("tl", tl)
  , ("cons?", consp)
  , ("+", mkArithFun (+))
  , ("-", mkArithFun (-))
  , ("*", mkArithFun (*))
  , ("/", mkArithFun (/))
  , (">", mkArithFun (>))
  , ("<", mkArithFun (<))
  , ("<=", mkArithFun (<=))
  , (">=", mkArithFun (>=))
  , ("number?", numberp)
  , ("set", set')
  , ("value", value)
  , ("intern", intern)
  , ("absvector", absvector)
  , ("address->", vecAssign)
  , ("<-address", vecRead)
  , ("absvector?", vectorp)
  , ("pr", pr)
  , ("read-byte", readByte)
  , ("open", open)
  , ("close", close)
  , ("=", eq)
  , ("simple-error", simpleError)
  , ("error-to-string", errorToString)
  ]
