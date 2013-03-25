{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module KLambda.Unparse (unparse) where

import KLambda.Types

import Text.Parsec hiding (string)
import qualified Text.Parsec.KLambda.Val as V

import Control.Applicative ((<*>), (<$>))

import Prelude hiding (exp)

anySymbolStr :: V.Parser String
anySymbolStr = do
    VSym (Symbol s) <- V.anySymbol
    return s

anySymbol :: V.Parser Symbol
anySymbol = Symbol <$> anySymbolStr

stringE, numE, boolE, symbolE, lambdaE, defunE, letE, appE, condE, ifE, exp :: V.Parser Exp

symbolE = ESym <$> anySymbolStr

stringE = do
    VStr s <- V.string
    return $ EStr s

numE = do
    VNum n <- V.num
    return $ ENum n

boolE = (V.symbol "true"  >> return (EBool True))
    <|> (V.symbol "false" >> return (EBool False))

lambdaE = V.listOf $ do
  V.symbol "lambda"
  ELambda <$> anySymbol <*> exp

defunE = V.listOf $ do
    V.symbol "defun"
    name <- anySymbol
    args <- V.listOf $ many anySymbol
    body <- exp
    return $ EDefun name (mkLambda args body)
  where mkLambda []     body = body
        mkLambda (a:as) body = ELambda a (mkLambda as body)

letE = V.listOf $ do
  V.symbol "let"
  name <- anySymbol
  val  <- exp
  body <- exp
  return $ EApp (ELambda name body) val

appE = V.listOf $ mkApp <$> exp <*> many exp
  where mkApp f [] = f
        mkApp f (a:as) = mkApp (EApp f a) as

condE = V.listOf $ V.symbol "cond" >> mkIf <$> many case_
  where
    case_ :: V.Parser (Exp, Exp)
    case_ = V.listOf $ (,) <$> exp <*> exp

    mkIf [(g, b)]    = EIf g b EUnit
    mkIf ((g, b):cs) = EIf g b (mkIf cs)

ifE = V.listOf $ do
  V.symbol "if"
  EIf <$> exp <*> exp <*> exp

exp = choice
  [ boolE, stringE, numE, symbolE, try lambdaE, try defunE, try letE, try condE
  , try ifE, try appE
  ] -- TODO: add unitE

unparse :: [Val] -> Either ParseError Exp
unparse = parse exp "klambda val"
