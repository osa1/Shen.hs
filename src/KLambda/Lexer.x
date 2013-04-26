{
module KLambda.Lexer where
}


%wrapper "posn"

$whitespace  = [ \ \t \r \n ]


$symbolstart = [ a-z A-Z \= \* \/ \+    \_ \? \$ \! \@ \~ \. \> \< \& \% \' \# \` \; \: \{ \} ]
$symbolrest  = [ a-z A-Z \= \* \/ \+ \- \_ \? \$ \! \@ \~ \. \> \< \& \% \' \# \` \; \: \{ \} ]
$digit       = 0-9
$neg         = \-

-- this is taken from my Lua lexer, make sure it works for KLambda
$str         = \0-\255 # [ \" ]

@mantpart    = \. $digit+


klambda :-

    $whitespace+                         ;
    \" $str* \"                          { \posn s -> (Str (tail . init $ s), Right posn) }
    \(                                   { \posn _ -> (LParen, Right posn) }
    \)                                   { \posn _ -> (RParen, Right posn) }

    $symbolstart [ $symbolrest $digit ]* { \posn s -> (Symbol s, Right posn) }
    $neg+                                { \posn s -> (Symbol s, Right posn) }
    $neg+ $symbolstart+ [ $symbolrest $digit ]*
                                         { \posn s -> (Symbol s, Right posn) }

    $neg? $digit+ @mantpart?             { \posn s -> (Number s, Right posn) }

{

data KlTok
    = Str String | LParen | RParen | Symbol String | Number String | EOF
    deriving (Show, Eq)

type KlToken = (KlTok, Either EOFPn AlexPosn)

mkTok :: KlTok -> (AlexPosn -> String -> KlToken)
mkTok tok = \posn _ -> (tok, Right posn)

data EOFPn = EOFPn deriving Show

alexScanTokens' str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> [(EOF, Left EOFPn)]
                AlexError ((AlexPn _ line column),_,_,_) ->
                  error $ concat [ "lexical error at "
                                 , show line
                                 , " line, "
                                 , show column
                                 , " column"
                                 ]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

}
