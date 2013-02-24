{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}
module KLambda.StdFun where

import qualified Data.HashMap.Strict as M

import KLambda.Types
import KLambda.Eval

klEnsureType :: Type -> [Exp] -> Kl Val
klEnsureType ty exps = do
    [e1] <- ensureArity 1 exps
    v1   <- eval e1
    return . klVal $ if typeOf v1 == ty
                       then True
                       else False

-- String operations
-- --------------------------------------------------------

pos, tlstr, cn, str, strp, nToStr, strToN :: [Exp] -> Kl Val
pos exps = do
    [e1, e2] <- ensureArity 2 exps
    VStr s   <- ensureType' e1 TyStr
    VNum n   <- ensureType' e2 TyNum
    return $ VStr [s !! (floor n)]

tlstr exps = do
    [e1]   <- ensureArity 1 exps
    VStr s <- ensureType' e1 TyStr
    return $ VStr (tail s)

cn exps = do
    [e1, e2] <- ensureArity 2 exps
    VStr s1  <- ensureType' e1 TyStr
    VStr s2  <- ensureType' e2 TyStr
    return $ VStr (s1 ++ s2)

str exps = do
    [e1] <- ensureArity 1 exps
    v    <- eval e1
    return $ VStr (show v)

strp = klEnsureType TyStr

nToStr = str

strToN exps = do
    [e1]   <- ensureArity 1 exps
    VStr n <- ensureType' e1 TyStr
    return $ VNum (read n)

-- Lists
-- --------------------------------------------------------

cons, hd, tl, consp :: [Exp] -> Kl Val
cons exps = do
    [e1, e2] <- ensureArity 2 exps
    v1       <- eval e1
    VList l  <- ensureType' e2 TyList
    return $ klVal (v1:l)

hd exps = do
    [e1]    <- ensureArity 1 exps
    VList l <- ensureType' e1 TyList
    return $ head l

tl exps = do
    [e1]    <- ensureArity 1 exps
    VList l <- ensureType' e1 TyList
    return $ klVal (tail l)

consp = klEnsureType TyList

-- Arithmetic
-- --------------------------------------------------------

mkArithFun :: forall a.  KlVal a => (Number -> Number -> a) -> [Exp] -> Kl Val
mkArithFun op exps = do
    [e1, e2] <- ensureArity 2 exps
    VNum n1  <- ensureType' e1 TyNum
    VNum n2  <- ensureType' e2 TyNum
    return $ klVal (n1 `op` n2)

numberp :: [Exp] -> Kl Val
numberp = klEnsureType TyNum

-- Standard environment
-- --------------------------------------------------------

stdEnv :: M.HashMap [Char] ([Exp] -> Kl Val)
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
  ]
