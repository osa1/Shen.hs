{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module KLambda.Fun where

import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V
import Control.Monad           (liftM, liftM2)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.State     (modify, gets)
import Control.Monad.Error     (throwError)
import System.IO               (IOMode(..), hGetChar, hClose, hPutStr, openFile)
import System.IO.Error         (tryIOError)

import KLambda.Types
import KLambda.Env

returnKl :: a -> Kl a
returnKl = return

type KlFun1 = Val -> Kl Val
type KlFun2 = Val -> KlFun1
type KlFun3 = Val -> KlFun2

klEnsureType :: Type -> KlFun1
klEnsureType ty v = returnKl . klVal $ typeOf v == ty

-- String operations
-- --------------------------------------------------------

pos :: KlFun2
pos v1 v2 = do
    s <- ensureType v1
    n :: Double <- ensureType v2
    return $ VStr [s !! floor n] :: Kl Val

tlstr :: KlFun1
tlstr v = do
    s <- ensureType v
    return $ VStr (tail s)

cn :: KlFun2
cn v1 v2 = do
    s1 <- ensureType v1
    s2 <- ensureType v2
    return $ VStr (s1 ++ s2)

str :: KlFun1
str v = returnKl $ VStr (show v)

strp :: KlFun1
strp = klEnsureType TyStr

nToStr :: KlFun1
nToStr v = do
    n :: Double <- ensureType v
    return $ VStr [toEnum $ floor n]

strToN :: KlFun1
strToN (VStr s) = return $ VNum . fromIntegral . sum $ map fromEnum s
strToN (VNum n) = return $ VNum n
strToN _        = error "strToN on some other value"

-- Lists
-- --------------------------------------------------------

cons :: KlFun2
cons v1 v2 = return $ VList v1 v2

hd :: KlFun1
hd v = case v of
         VList v1 _ -> return v1
         notVList   -> throwError TypeError{ foundTy = typeOf notVList, expectedTy = TyList }

tl :: KlFun1
tl v = case v of
         VList _ v2 -> return v2
         notVList   -> throwError TypeError{ foundTy = typeOf notVList, expectedTy = TyList }

consp :: KlFun1
consp v = return . VBool $ case v of
                             VList{} -> True
                             _       -> False

-- Arithmetic
-- --------------------------------------------------------

mkArithFun :: forall a. KlVal a => (Double -> Double -> a) -> Func
mkArithFun op = StdFun $ \v1 v2 -> do
  n1 <- ensureType v1
  n2 <- ensureType v2
  return $ klVal (n1 `op` n2)

numberp :: Func
numberp = StdFun $ klEnsureType TyNum

-- Assignments
-- --------------------------------------------------------

set' :: KlFun2
set' v1 v2 = do
    s <- ensureType v1
    modify $ \env -> insertSymEnv s v2 env
    return v2

value :: KlFun1
value v = do
  Symbol s <- ensureType v
  senv     <- gets symEnv
  case M.lookup s senv of
    Nothing -> throwError $ UnboundSymbol (Symbol s)
    Just v' -> return v'

-- Symbols
-- --------------------------------------------------------

intern :: KlFun1
intern v = do
    s <- ensureType v
    return $ (VSym . Symbol) s

-- Vectors
-- --------------------------------------------------------

absvector :: KlFun1
absvector v = do
    n   <- ensureType v
    vec <- liftIO $ MV.new n
    return $ VVec vec

vecAssign :: KlFun3
vecAssign vec idx val = do
    vec' <- ensureType vec
    idx' <- ensureType idx
    -- TODO: can we assume that Shen implementation in KLambda already does
    -- the bound checking? if so we can use `unsafeWrite`.
    liftIO $ MV.write vec' idx' val
    return vec

vecRead :: KlFun2
vecRead vec idx = do
    vec' <- ensureType vec
    idx' <- ensureType idx
    -- TODO: same situation with `vecAssign`. `unsafeRead` can be used.
    liftIO (MV.read vec' idx' :: IO Val)

vectorp :: KlFun1
vectorp = klEnsureType TyVec

-- Error handling
-- --------------------------------------------------------

simpleError :: KlFun1
simpleError s = do
    msg <- ensureType s
    throwError $ UserError $ UserErrorMsg msg

errorToString :: KlFun1
errorToString e = do
    UserErrorMsg err <- ensureType e
    return $ VStr err

-- Streams and I/O
-- --------------------------------------------------------

pr :: KlFun2
pr s stream = do
    s' <- ensureType s
    handle <- ensureType stream
    liftIO $ hPutStr handle s'
    return $ VStr s'

readByte :: KlFun1
readByte stream = do
    handle <- ensureType stream
    byte <- liftIO $ tryIOError (hGetChar handle)
    return . VNum $ case byte of
                      Left _  -> -1
                      Right b -> fromIntegral $ fromEnum b

open :: KlFun3
open _ path dir = do
    path' <- ensureType path
    Symbol dir' <- ensureType dir
    mode <- case dir' of
              "in"  -> return ReadMode
              "out" -> return WriteMode
                -- TODO: maybe I should ensure this part and encode it in syntax tree
              wrong -> throwError $ ErrMsg ("invalid open mode: " ++ wrong)
    handle <- liftIO $ openFile path' mode
    return $ VStream handle

close :: KlFun1
close stream = do
  handle <- ensureType stream
  liftIO $ hClose handle
  return $ VUnit

-- Streams and I/O
-- --------------------------------------------------------

eq :: KlFun2
eq v1 v2 = liftM VBool $ eq' v1 v2
  where
    eq' :: Val -> Val -> Kl Bool
    eq' (VSym s1)  (VSym s2)  = return $ s1 == s2
    eq' (VBool b1) (VBool b2) = return $ b1 == b2
    eq' (VStr s1)  (VStr s2)  = return $ s1 == s2
    eq' (VNum n1)  (VNum n2)  = return $ n1 == n2
    eq' (VList v1 v2) (VList v1' v2') = liftM2 (&&) (eq' v1 v1') (eq' v2 v2')
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
    eq' VUnit{}    VUnit{}    = return True
    eq' _          _          = return False

-- Standard environment
-- --------------------------------------------------------

stdenv :: M.HashMap String Func
stdenv = M.fromList
  [ ("pos", StdFun pos)
  , ("tlstr", StdFun tlstr)
  , ("cn", StdFun cn)
  , ("str", StdFun str)
  , ("string?", StdFun strp)
  , ("n->string", StdFun nToStr)
  , ("string->n", StdFun strToN)
  , ("cons", StdFun cons)
  , ("hd", StdFun hd)
  , ("tl", StdFun tl)
  , ("cons?", StdFun consp)
  , ("+", mkArithFun (+))
  , ("-", mkArithFun (-))
  , ("*", mkArithFun (*))
  , ("/", mkArithFun (/))
  , (">", mkArithFun (>))
  , ("<", mkArithFun (<))
  , ("<=", mkArithFun (<=))
  , (">=", mkArithFun (>=))
  , ("number?", numberp)
  , ("set", StdFun set')
  , ("value", StdFun value)
  , ("intern", StdFun intern)
  , ("absvector", StdFun absvector)
  , ("address->", StdFun vecAssign)
  , ("<-address", StdFun vecRead)
  , ("absvector?", StdFun vectorp)
  , ("simple-error", StdFun simpleError)
  , ("error-to-string", StdFun errorToString)
  , ("pr", StdFun pr)
  , ("read-byte", StdFun readByte)
  , ("open", StdFun open)
  , ("close", StdFun close)
  , ("=", StdFun eq)
  ]
