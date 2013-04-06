{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Text.Parsec.KLambda.Val where

import KLambda.Types

import Text.Parsec hiding (satisfy)

type Parser = Parsec [Val] ()

satisfy :: Stream [Val] m Val => (Val -> Bool) -> ParsecT [Val] u m Val
satisfy f = tokenPrim show nextPos valeq'
  where
    valeq' :: Val -> Maybe Val
    valeq' v = if f v then Just v else Nothing

    nextPos :: SourcePos -> Val -> [Val] -> SourcePos
    nextPos p _ _ = p

valeq :: Val -> Val -> Bool
valeq (VSym (Symbol s)) (VSym (Symbol s')) = s == s'
valeq (VBool b)  (VBool b') = b == b'
valeq (VStr s)   (VStr s')  = s == s'
valeq (VNum n)   (VNum n')  = n == n'
valeq (VList v1 v2) (VList v1' v2') = valeq v1 v1' && valeq v2 v2'
valeq (VFun f1)  (VFun f2)  = error $ concat
    [ "can't compare function values ", show f1, " and ", show f2, "." ]
valeq VVec{}     VVec{}     = error "can't compare vector values"
valeq VStream{}  VStream{}  = error "can't compare stream values"
valeq _          _          = False

listOf :: Parsec [Val] () b -> Parsec [Val] () b
listOf p = do
    vlist <- satisfy pred
    let lst = listOfVList vlist
    let parsetarget = case last lst of
                        VUnit{} -> init lst
                        _ -> lst
    case parse p "unparse" parsetarget of
      Right r  -> return r
      Left err -> fail $ show err
  where pred VList{} = True
        pred _       = False

tok :: Stream [Val] m Val => Val -> ParsecT [Val] u m Val
tok t = satisfy (valeq t) <?> show t

string, num, anySymbol :: Monad m => ParsecT [Val] u m Val

string = satisfy p <?> "string"
  where p t = case t of VStr _ -> True
                        _ -> False

num = satisfy p <?> "number"
  where p t = case t of VNum _ -> True
                        _ -> False

anySymbol = satisfy p <?> "symbol"
  where p t = case t of VSym _ -> True
                        _ -> False

symbol :: Monad m => String -> ParsecT [Val] u m Val
symbol s = satisfy p <?> "symbol \"" ++ s ++ "\""
  where p t = case t of VSym (Symbol s') -> s == s'
                        _ -> False
