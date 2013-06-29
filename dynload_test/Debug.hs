module Debug where

import           KLambda.Parser
import           KLambda.Types

import           Control.Monad.IO.Class (liftIO)
import           Prelude                hiding (exp, read)
import           Text.Parsec

-- | Parse parameter using KLambda parser and print result. Returns unit.
parseVal :: KlFun1
parseVal val = do
    liftIO $ print val
    case parse exp "parseVal" [val] of
      Left err  -> liftIO $ print err
      Right exp -> liftIO $ print exp
    return VUnit

-- | Print first parameter and return second parameter. It's used to trace
-- function calls.
debugPrint :: KlFun2
debugPrint str val = do
    str' <- ensureType str
    liftIO $ putStrLn str'
    return val

-- | Print second parameter appended to first parameter(a string) and
-- return second parameter. It's used to trace evaluated expressions.
debug :: KlFun2
debug str val = do
    str' <- ensureType str
    liftIO $ putStrLn (str' ++ show val ++ "\n")
    return val

-- | Print first parameter, then second parameter(with it's type) and
-- return third parameter.
debug1 :: KlFun3
debug1 str val retval = do
    str' <- ensureType str
    liftIO $ putStrLn $ concat [ str', show val, " :: ", show $ typeOf val, "\n" ]
    return retval

-- Foreign interface

parseValF, debugPrintF, debugF, debug1F :: KlFun1
parseValF   = mkFun1 parseVal
debugPrintF = mkFun1 debugPrint
debugF      = mkFun1 debug
debug1F     = mkFun1 debug1
