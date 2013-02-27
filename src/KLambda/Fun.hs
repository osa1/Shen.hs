{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Fun where

import qualified Data.HashMap.Strict as M
import Control.Monad.State (modify, gets)
import Data.Maybe (fromJust)

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
  ]
