{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module KLambda.Fun where

import           KLambda.Env
import           KLambda.Parser
import           KLambda.Types
import           KLambda.Vector

import           Control.Monad          (liftM, liftM2, zipWithM)
import           Control.Monad.Error    (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get, gets, modify)
import qualified Data.HashMap.Strict    as M
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Prelude                hiding (exp, read)
import           System.FilePath        ((</>))
import           System.IO              (IOMode (..), hClose, hFlush, hGetChar,
                                         hPutStr, openFile)
import           System.IO.Error        (tryIOError)
import           Text.Parsec

returnKl :: a -> Kl a
returnKl = return

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
str v = return $ VStr (show v)

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
    vec <- liftIO $ newVector n
    return $ VVec vec

vecAssign :: KlFun3
vecAssign vec idx val = do
    vec' <- ensureType vec
    idx' <- ensureType idx
    -- TODO: can we assume that Shen implementation in KLambda already does
    -- the bound checking? if so we can use `unsafeWrite`.
    ret <- liftIO $ write vec' idx' val
    case ret of
      Left exc -> throwError $ VectorErr exc
      Right _ -> return vec

vecRead :: KlFun2
vecRead vec idx = do
    vec' <- ensureType vec
    idx' <- ensureType idx
    -- TODO: same situation with `vecAssign`. `unsafeRead` can be used.
    ret <- liftIO (read vec' idx')
    case ret of
      Left exc -> throwError $ VectorErr exc
      Right Nothing -> throwError $ VectorErr (NotInitialized idx')
      Right (Just v) -> return v

vectorp :: KlFun1
vectorp = klEnsureType TyVec

-- Error handling
-- --------------------------------------------------------

simpleError :: KlFun1
simpleError s = do
    msg <- ensureType s
    throwError $ UserError msg

errorToString :: KlFun1
errorToString e = do
    err :: KlException <- ensureType e
    return $ VStr (show err)

-- Streams and I/O
-- --------------------------------------------------------

pr :: KlFun2
pr s stream = do
    s' <- ensureType s
    handle <- ensureType stream
    liftIO $ do
      hPutStr handle s'
      hFlush handle
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
    env <- get
    homedir <- case lookupSym (Symbol "*home-directory*") env of
                 Nothing -> error "*home-directory* is not set"
                 Just p  -> ensureType p
    handle <- liftIO $ openFile (homedir </> path') mode
    return $ VStream handle

close :: KlFun1
close stream = do
  handle <- ensureType stream
  liftIO $ hClose handle
  return VUnit

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
    eq' (VVec v1)  (VVec v2)  = do
      v1' <- liftIO $ vectorToList v1
      v2' <- liftIO $ vectorToList v2
      liftM and $ zipWithM eq'' v1' v2'
      where
        eq'' :: Maybe Val -> Maybe Val -> Kl Bool
        eq'' Nothing Nothing = return True
        eq'' (Just v1) (Just v2) = eq' v1 v2
        eq'' _ _ = return False
    eq' VStream{}  VStream{}  = return False
    eq' VUnit{}    VUnit{}    = return True
    eq' _          _          = return False

-- Time
-- --------------------------------------------------------

getTime :: KlFun1
getTime _ = do
  t <- liftIO getPOSIXTime
  return $ VNum . fromIntegral . fromEnum $ t

-- Debugging
-- --------------------------------------------------------

parseVal :: KlFun1
parseVal val = do
  liftIO $ print val
  case parse exp "parseVal" [val] of
    Left err -> liftIO $ print err
    Right exp -> liftIO $ print exp
  return VUnit

debug :: KlFun2
debug str val = do
    str' <- ensureType str
    liftIO $ putStrLn (str' ++ show val ++ "\n")
    return val

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
  , ("get-time", StdFun getTime)
  , ("parse-val", StdFun parseVal)
  , ("debug", StdFun debug)
  ]
