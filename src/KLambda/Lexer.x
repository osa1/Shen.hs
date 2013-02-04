{
module Lexer where
}


%wrapper "posn"

$whitespace  = [ \ \t \r \n ]
$alpha       = [ a-z A-Z \= \- \* \/ \+ \_ \? \$ \! \@ \~ \. \> \< \& \% \' \# \` \; \: \{ \} ]
$digit       = 0-9

-- this is taken from my Lua lexer, make sure it works for KLambda
$str         = \0-\255 # [ \" ]

shen :-

    $whitespace+              ;
    "let"                     { mkTok TLet }
    "true"                    { \posn _ -> (TBool True, Right posn) }
    "false"                   { \posn _ -> (TBool False, Right posn) }
    "lambda"                  { mkTok TLambda }
    "defun"                   { mkTok TDefun }
    \" $str* \"               { \posn s -> (TStr s, Right posn) }
    \(                        { \posn _ -> (TLParen, Right posn) }
    \)                        { \posn _ -> (TRParen, Right posn) }
    $alpha                    { \posn s -> (TSymbol s, Right posn) }
    $alpha [ $alpha $digit ]+ { \posn s -> (TSymbol s, Right posn) }
    $digit+                   { \posn s -> (TNumber s, Right posn) }

{

data KlTok
    = TStr String | TLParen | TRParen | TSymbol String | TNumber String
    | TBool Bool | TLambda | TDefun | TLet | TEOF
    deriving (Show, Eq)

type KlToken = (KlTok, Either EOFPn AlexPosn)

mkTok :: KlTok -> (AlexPosn -> String -> KlToken)
mkTok tok = \posn _ -> (tok, Right posn)

data EOFPn = EOFPn deriving Show

alexScanTokens' str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> [(TEOF, Left EOFPn)]
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

}
