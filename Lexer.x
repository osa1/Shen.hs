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
    \" $str* \"               { TString }
    \(                        { \posn _ -> TLParen posn }
    \)                        { \posn _ -> TRParen posn }
    $alpha                    { TSymbol }
    $alpha [ $alpha $digit ]+ { TSymbol }
    $digit+                   { TNumber }

{

data Token = TString AlexPosn String
           | TLParen AlexPosn
           | TRParen AlexPosn
           | TSymbol AlexPosn String
           | TNumber AlexPosn String
    deriving Show

}
