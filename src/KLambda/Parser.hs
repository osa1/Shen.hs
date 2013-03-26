{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module KLambda.Parser where

import qualified KLambda.Lexer as L
import KLambda.Types

import Text.Parsec hiding (string)
import qualified Text.Parsec.KLambda.Exp as Exp
import qualified Text.Parsec.KLambda.Val as Val

import Control.Applicative ((<*>), (<$>), (<*))

import Prelude hiding (exp)

class KLambdaParser tokpos tok | tokpos -> tok where
    tok       :: tok -> Parsec [tokpos] () tok
    string    :: Parsec [tokpos] () String
    num       :: Parsec [tokpos] () Number
    anySymbol :: Parsec [tokpos] () Symbol
    symbol    :: String -> Parsec [tokpos] () tok
    unit      :: Parsec [tokpos] () ()

    listOf    :: Parsec [tokpos] () a -> Parsec [tokpos] () a

instance KLambdaParser L.KlToken L.KlTok where
    tok = Exp.tok

    string = do
      L.Str s <- Exp.string
      return s

    num = do
      L.Number s <- Exp.num
      return $ read s

    anySymbol = do
      L.Symbol s <- Exp.anySymbol
      return (Symbol s)

    symbol = Exp.symbol

    unit = tok L.LParen >> tok L.RParen >> return ()

    listOf = Exp.listOf

instance KLambdaParser Val Val where
    tok = Val.tok

    string = do
      VStr s <- Val.string
      return s

    num = do
      VNum n <- Val.num
      return n

    anySymbol = do
      VSym s <- Val.anySymbol
      return s

    symbol = Val.symbol

    unit = Val.satisfy f >> return ()
      where f (VList []) = True
            f _          = False

    listOf = Val.listOf

stringE, numE, boolE, symbolE, lambdaE, defunE, letE, appE, condE, ifE
  :: KLambdaParser tokpos a => Parsec [tokpos] () Exp

stringE = EStr <$> string

numE = ENum <$> num

boolE = (symbol "true"  >> return (EBool True))
    <|> (symbol "false" >> return (EBool False))

symbolE = do
    Symbol s <- anySymbol
    return $ ESym s

lambdaE = listOf $ do
  symbol "lambda"
  ELambda <$> anySymbol <*> exp

defunE = listOf $ do
    symbol "defun"
    name <- anySymbol
    args <- listOf $ many anySymbol
    body <- exp
    let binding = if null args
                    then ELambda (Symbol "__p__") body
                    else mkLambda args body
    return $ EDefun name binding
  where mkLambda []     body = body
        mkLambda (a:as) body = ELambda a (mkLambda as body)

letE = listOf $ do
  symbol "let"
  name <- anySymbol
  val  <- exp
  body <- exp
  return $ EApp (ELambda name body) val

appE = listOf $ do
    fun  <- exp
    args <- many exp
    return $ if null args
               then EApp fun EUnit
               else mkApp fun args
  where mkApp f [] = f
        mkApp f (a:as) = mkApp (EApp f a) as

condE = listOf $ symbol "cond" >> mkIf <$> many case_
  where case_ = listOf $ (,) <$> exp <*> exp
        mkIf [(g, b)]    = EIf g b EUnit
        mkIf ((g, b):cs) = EIf g b (mkIf cs)

ifE = listOf $ do
  symbol "if"
  EIf <$> exp <*> exp <*> exp

exp :: KLambdaParser tokpos a => Parsec [tokpos] () Exp
exp = choice
  [ boolE, stringE, numE, symbolE, try lambdaE, try defunE, try letE, try condE
  , try ifE, try appE, unit >> return EUnit
  ]

exps :: KLambdaParser tokpos a => Parsec [tokpos] () [Exp]
exps = many exp

parseText :: String -> IO ()
parseText = parseTest (many exp <* tok L.EOF) . L.alexScanTokens'

parseText' :: Show a => Parsec [L.KlToken] () a -> String -> IO ()
parseText' p = parseTest p . L.alexScanTokens'
