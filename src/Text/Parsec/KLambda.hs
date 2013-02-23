{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Text.Parsec.KLambda where

import KLambda.Lexer

import Text.Parsec hiding (satisfy)

type Parser = Parsec [KlToken] ()

satisfy :: (Stream [KlToken] m KlToken) => (KlToken -> Bool) -> ParsecT [KlToken] u m KlTok
satisfy f = tokenPrim show nextPos tokeq
  where nextPos :: SourcePos -> KlToken -> [KlToken] -> SourcePos
        nextPos pos _ ((_, (Right (AlexPn _ l c))):_) = setSourceColumn (setSourceLine pos l) c
        nextPos pos _ ((_, (Left _)):_)               = pos
        nextPos pos _ []                              = pos

        tokeq :: KlToken -> Maybe KlTok
        tokeq t = if f t then Just (fst t) else Nothing

tok :: (Stream [KlToken] m KlToken) => KlTok -> ParsecT [KlToken] u m KlTok
tok t = satisfy (\(t', _) -> t' == t) <?> show t

string, num, anySymbol :: Monad m => ParsecT [KlToken] u m KlTok

string = satisfy p <?> "string"
  where p (t, _) = case t of Str _ -> True
                             _ -> False

num = satisfy p <?> "number"
  where p (t, _) = case t of Number _ -> True
                             _ -> False

anySymbol = satisfy p <?> "symbol"
  where p (t, _) = case t of Symbol _ -> True
                             _ -> False

symbol :: Monad m => String -> ParsecT [KlToken] u m KlTok
symbol s = satisfy p <?> "symbol \"" ++ s ++ "\""
  where p (t, _) = case t of Symbol s' -> if s' == s then True else False
                             _ -> False
