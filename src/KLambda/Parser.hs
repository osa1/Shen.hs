{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module KLambda.Parser where

import qualified KLambda.Lexer as L
import KLambda.Types

import Text.Parsec hiding (string)
import qualified Text.Parsec.KLambda.Exp as KL

import Control.Applicative ((<*>), (<$>), (<*))

import Prelude hiding (exp)

parens :: KL.Parser a -> KL.Parser a
parens = between (KL.tok L.LParen) (KL.tok L.RParen)

anySymbol :: KL.Parser Symbol
anySymbol = do
    L.Symbol s <- KL.anySymbol
    return (Symbol s)

stringE, numE, boolE, symbolE, lambdaE, defunE, letE, appE, unitE,
  condE, ifE, exp :: KL.Parser Exp
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
    let binding = if null args
                    then ELambda (Symbol "__p__") body
                    else mkLambda args body
    return $ EDefun name binding
  where mkLambda []     body = body
        mkLambda (a:as) body = ELambda a (mkLambda as body)

letE = parens $ do
  KL.symbol "let"
  name <- anySymbol
  val  <- exp
  body <- exp
  return $ EApp (ELambda name body) val

appE = parens $ do
    fun <- exp
    args <- many exp
    return $ if null args
               then EApp fun EUnit
               else mkApp fun args
  where mkApp f [] = f
        mkApp f (a:as) = mkApp (EApp f a) as

unitE = KL.tok L.LParen >> KL.tok L.RParen >> return EUnit

condE = parens $ KL.symbol "cond" >> mkIf <$> many case_
  where case_ = parens $ (,) <$> exp <*> exp
        mkIf [(g, b)]    = EIf g b EUnit
        mkIf ((g, b):cs) = EIf g b (mkIf cs)

ifE = parens $ do
  KL.symbol "if"
  EIf <$> exp <*> exp <*> exp

exp = choice
  [ boolE, stringE, numE, symbolE, try lambdaE, try defunE, try letE, try condE
  , try ifE, try appE, unitE
  ]

exps :: KL.Parser [Exp]
exps = many exp

parseText :: String -> IO ()
parseText = parseTest (many exp <* KL.tok L.EOF) . L.alexScanTokens'

parseText' :: (Show a) => KL.Parser a -> String -> IO ()
parseText' p = parseTest p . L.alexScanTokens'
