{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module KLambda.Fun where

import qualified Data.HashMap.Strict as M
import Control.Monad.State (modify, gets)
import Control.Monad.Error (throwError)
import Data.Maybe (fromJust)

import KLambda.Types
import KLambda.Eval
import KLambda.Env

klEnsureType :: Type -> Func
klEnsureType ty = StdFun $ \e -> do
  v1 <- eval e
  return . klVal $ if typeOf v1 == ty
                     then True
                     else False

instance KlFun Func where
    apply (Closure env argName body) arg = do
      arg' <- eval arg
      evalWEnv (insertSymEnv argName arg' env) body
    apply (StdFun f) arg = apply f arg

instance KlFun (Kl Val) where
    apply v arg = do
      v' <- v
      case v' of
        VFun f -> apply f arg
        VSym (Symbol s) -> do
          fenv <- gets fst
          apply (fromJust $ M.lookup s fenv) arg
        inv ->
          throwError $ TypeError { expectedTy = TyFun, foundTy = typeOf inv }

instance KlVal (Exp -> Kl Val) where klVal = VFun . StdFun

-- String operations
-- --------------------------------------------------------

pos, tlstr, cn, str, strp, nToStr, strToN :: Func
pos = StdFun $ \e1 e2 -> do
  s             <- ensureType =<< eval e1
  (n :: Double) <- ensureType =<< eval e2
  return $ VStr [s !! (floor n)]

tlstr = StdFun $ \e -> do
  s <- ensureType =<< eval e
  return $ VStr (tail s)

cn = StdFun $ \e1 e2 -> do
  s1 <- ensureType =<< eval e1
  s2 <- ensureType =<< eval e2
  return $ VStr (s1 ++ s2)

str = StdFun $ \e -> do
  v <- eval e
  return $ VStr (show v)

strp = klEnsureType TyStr

nToStr = str

strToN = StdFun $ \e -> do
  n <- ensureType =<< eval e
  return $ VNum (read n)

-- Lists
-- --------------------------------------------------------

cons, hd, tl, consp :: Func
cons = StdFun $ \e1 e2 -> do
  vl <- eval e1
  l  <- ensureType =<< eval e2
  return $ klVal (vl:l)

hd = StdFun $ \e -> do
  (l :: [Val]) <- ensureType =<< eval e
  return $ head l

tl = StdFun $ \e -> do
  (l :: [Val]) <- ensureType =<< eval e
  return $ klVal (tail l)

consp = klEnsureType TyList

-- Arithmetic
-- --------------------------------------------------------

mkArithFun :: forall a. KlVal a => (Double -> Double -> a) -> Func
mkArithFun op = StdFun $ \e1 e2 -> do
  n1 <- ensureType =<< eval e1
  n2 <- ensureType =<< eval e2
  return $ klVal (n1 `op` n2)

numberp :: Func
numberp = klEnsureType TyNum

-- Assignments
-- --------------------------------------------------------

set', value :: Func
set' = StdFun $ \e1 e2 -> do
  s <- ensureType =<< eval e1
  v <- eval e2
  modify $ \env -> insertSymEnv s v env
  return v

value = StdFun $ \e -> do
  s    <- ensureType =<< eval e
  senv <- gets symEnv
  return $ fromJust (M.lookup s senv)

-- Symbols
-- --------------------------------------------------------

intern :: Func
intern = StdFun $ \e -> do
  s <- ensureType =<< eval e
  return $ VSym s

-- Standard environment
-- --------------------------------------------------------

stdEnv :: M.HashMap String Func
stdEnv = M.fromList
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
