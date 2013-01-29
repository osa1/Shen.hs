{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parser where

import Lexer
import Text.Parsec hiding (string)
import qualified Text.Parsec.KLambda as KL
import Data.Maybe (fromMaybe)

import Control.Applicative ((<*>), (<$>), (<*))

newtype Symbol = Symbol String deriving Show

data Sexp
    = Sym Symbol
    | Bool Bool
    | String String
    | Number String
    | Lambda Symbol Sexp
    | Let Symbol Sexp Sexp
    | Defun Symbol [Symbol] Sexp
    | App Sexp [Sexp]
    | Unit
    deriving Show

parens :: KL.Parser a -> KL.Parser a
parens = between (KL.tok TLParen) (KL.tok TRParen)

string, num, bool, symbol, lambda, letexp, defun, app, unit :: KL.Parser Sexp
string = do
    TStr s <- KL.string
    return $ String s

num = do
    TNumber n <- KL.num
    return $ Number n

bool = do
    TBool b <- KL.bool
    return $ Bool b

symbol' :: KL.Parser Symbol
symbol' = do
    TSymbol s <- KL.symbol
    return $ Symbol s

symbol = Sym <$> symbol'

lambda = parens $ do
    KL.tok TLambda
    arg <- symbol'
    body <- sexp
    return $ Lambda arg body

letexp = parens $ Let <$> (KL.tok TLet >> symbol') <*> sexp <*> sexp

defun = parens $
  Defun <$> (KL.tok TDefun >> symbol') <*> parens (many symbol') <*> sexp

app = parens $ do
    fun <- sexp
    args <- fromMaybe [] <$> (optionMaybe $ many1 sexp)
    return $ App fun args

unit = KL.tok TLParen >> KL.tok TRParen >> return Unit

sexp :: KL.Parser Sexp
sexp = choice [ string, num, try bool, symbol, try lambda, try letexp, try defun, try app, unit ]

parseText :: String -> IO ()
parseText = parseTest (sexp <* KL.tok TEOF) . alexScanTokens'

parseText' :: (Show a) => KL.Parser a -> String -> IO ()
parseText' p = parseTest p . alexScanTokens'
