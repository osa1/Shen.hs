module Debug where

import           KLambda.Parser
import           KLambda.Types

import           Control.Monad.IO.Class (liftIO)
import           Prelude                hiding (exp, read)
import           Text.Parsec

parseVal :: KlFun1
parseVal val = do
    liftIO $ print val
    case parse exp "parseVal" [val] of
      Left err  -> liftIO $ print err
      Right exp -> liftIO $ print exp
    return VUnit

debugPrint :: KlFun2
debugPrint str val = do
    str' <- ensureType str
    liftIO $ putStrLn str'
    return val

debug :: KlFun2
debug str val = do
    str' <- ensureType str
    liftIO $ putStrLn (str' ++ show val ++ "\n")
    return val

debug1 :: KlFun3
debug1 str val retval = do
    str' <- ensureType str
    liftIO $ putStrLn $ concat [ str', show val, " :: ", show $ typeOf val, "\n" ]
    return retval

test :: KlFun0
test = do
    liftIO $ putStrLn "test"
    return VUnit

-- Foreign interface

parseValF, debugPrintF, debugF, debug1F, testF :: KlFun1
parseValF   = mkFun1 parseVal
debugPrintF = mkFun1 debugPrint
debugF      = mkFun1 debug
debug1F     = mkFun1 debug1
testF       = mkFun1 test
