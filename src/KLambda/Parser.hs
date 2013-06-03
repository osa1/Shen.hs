{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module KLambda.Parser where

import qualified KLambda.Lexer           as L
import           KLambda.Types

import           Control.Applicative     ((<$>), (<*), (<*>))
import           Control.Monad           (void)
import           Prelude                 hiding (exp)
import           Text.Parsec             hiding (string)
import qualified Text.Parsec.KLambda.Exp as Exp
import qualified Text.Parsec.KLambda.Val as Val

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

    unit = void $ tok L.LParen >> tok L.RParen

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

    unit = void $ Val.satisfy f
      where
        f VUnit{} = True
        f _       = False

    listOf = Val.listOf

stringE, numE, boolE, symbolE, lambdaE, defunE, letE, appE, condE, orE, andE, ifE
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
  ELambda <$> (Just <$> anySymbol) <*> exp

defunE = listOf $ do
    symbol "defun"
    name <- anySymbol
    args <- listOf $ many anySymbol
    body <- exp
    let binding = if null args
                    then ELambda Nothing body
                    else mkLambda args body
    return $ EDefun name binding
  where
    mkLambda []     body = body
    mkLambda (a:as) body = ELambda (Just a) (mkLambda as body)

letE = listOf $ do
  symbol "let"
  name <- anySymbol
  val  <- exp
  body <- exp
  return $ EApp (ELambda (Just name) body) (Just val)

appE = listOf $ do
    fun  <- exp
    args <- many exp
    return $ if null args
               then EApp fun Nothing
               else mkApp fun args
  where
    mkApp f [] = f
    mkApp f (a:as) = mkApp (EApp f (Just a)) as

condE = listOf $ symbol "cond" >> mkIf <$> many case_
  where
    case_ = listOf $ (,) <$> exp <*> exp
    mkIf [(g, b)]    = EIf g b EUnit
    mkIf ((g, b):cs) = EIf g b (mkIf cs)

orE = listOf $ do
  symbol "or"
  e1 <- exp
  e2 <- exp
  return $ EIf e1 (EBool True) e2

-- TODO: maybe I should implement andE and orE as a special form like
-- eval-kl to keep parser simple
andE = listOf $ do
  symbol "and"
  e1 <- exp
  e2 <- exp
  return $ EIf e1 e2 (EBool False)

ifE = listOf $ do
  symbol "if"
  EIf <$> exp <*> exp <*> exp

exp :: KLambdaParser tokpos a => Parsec [tokpos] () Exp
exp = choice
  [ boolE, stringE, numE, symbolE, try lambdaE, try defunE, try letE, try condE
  , try orE, try andE, try ifE, try appE, unit >> return EUnit
  ]

exps :: KLambdaParser tokpos a => Parsec [tokpos] () [Exp]
exps = many exp

parseText :: String -> IO ()
parseText = parseTest (many exp <* tok L.EOF) . L.alexScanTokens'

parseText' :: Show a => Parsec [L.KlToken] () a -> String -> IO ()
parseText' p = parseTest p . L.alexScanTokens'
