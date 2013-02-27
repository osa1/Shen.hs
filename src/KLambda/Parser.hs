module KLambda.Parser where

import qualified KLambda.Lexer as L
import KLambda.Types

import Text.Parsec hiding (string)
import qualified Text.Parsec.KLambda as KL

import Control.Applicative ((<*>), (<$>), (<*))
import Data.Maybe (fromMaybe)
import Control.Monad (void)

import Prelude hiding (exp)

parens :: KL.Parser a -> KL.Parser a
parens = between (KL.tok L.LParen) (KL.tok L.RParen)

anySymbol :: KL.Parser Symbol
anySymbol = do
    L.Symbol s <- KL.anySymbol
    return s

stringE, numE, boolE, symbolE, lambdaE, defunE, letE, appE, unitE, condE :: KL.Parser Exp
stringE = do
    L.Str s <- KL.string
    return $ EStr s

numE = do
    L.Number n <- KL.num
    return $ ENum (read n)

boolE = (KL.symbol "true"  >> return (EBool True))
    <|> (KL.symbol "false" >> return (EBool False))

symbol' :: KL.Parser String
symbol' = do
    L.Symbol s <- KL.anySymbol
    return s

symbolE = ESym <$> symbol'

lambdaE = parens $ do
  KL.symbol "lambda"
  ELambda <$> anySymbol <*> exp

defunE = parens $ do
    KL.symbol "defun"
    name <- anySymbol
    args <- parens $ many anySymbol
    body <- exp
    return $ EApp (EApp (ESym "set") (ESym name)) (mkLambda args body)
  where mkLambda []     body = body
        mkLambda (a:as) body = ELambda a (mkLambda as body)

letE = parens $ do
  KL.symbol "let"
  name <- anySymbol
  val  <- exp
  body <- exp
  return $ EApp (ELambda name body) val

appE = parens $ EApp <$> exp <*> many exp

unitE = KL.tok L.LParen >> KL.tok L.RParen >> return EUnit

condE = parens $ do
    KL.symbol "cond"
    ECond <$> many case_
  where case_ = parens $ (,) <$> exp <*> exp

exp = choice
  [ boolE, stringE, numE, symbolE, try lambdaE, try defunE, try letE, try condE, try appE, unitE ]

parseText :: String -> IO ()
parseText = parseTest (many exp <* KL.tok L.EOF) . L.alexScanTokens'

parseText' :: (Show a) => KL.Parser a -> String -> IO ()
parseText' p = parseTest p . L.alexScanTokens'
