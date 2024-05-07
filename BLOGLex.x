{
module BLOGLex (main,lexer,Token (..)) where
import Prelude hiding (GT, LT, EQ)  -- clashes with Token type
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n]
$char = [a-zA-Z]

@ident = (\_ | $alpha) ($alpha | \_ | $digit)*

@singlelinecomment = \/\/ .* $newline

@contents = ([^\/] | $newline)* (\/ ([^\*] | $newline)+)*
@multilinecomment  = \/\* @contents \*\/

tokens :-
  $white+                        ;
  @singlelinecomment             ;
  @multilinecomment              ;
  \-\>                           { \s -> RIGHTARROW }
  \=\>                           { \s -> DOUBLERIGHTARROW }
  \(                             { \s -> LPAREN }
  \)                             { \s -> RPAREN }
  \{                             { \s -> LBRACE }
  \}                             { \s -> RBRACE }
  \[                             { \s -> LBRACKET }
  \]                             { \s -> RBRACKET }
  \;                             { \s -> SEMI }
  \~                             { \s -> DISTRIB }
  \:                             { \s -> COLON }
  \@                             { \s -> AT }
  \=                             { \s -> BLOGLex.EQ }
  \,                             { \s -> COMMA }
  \.                             { \s -> DOT }
  \#                             { \s -> NUMSIGN }
  if                             { \s -> IF }
  then                           { \s -> THEN }
  else                           { \s -> ELSE }
  type                           { \s -> TYPE }
  obs                            { \s -> OBS }
  query                          { \s -> QUERY }
  distinct                       { \s -> DISTINCT }
  fixed                          { \s -> FIXED }
  random                         { \s -> RANDOM }
  origin                         { \s -> ORIGIN }
  null                           { \s -> NULL }
  list                           { \s -> LIST }
  map                            { \s -> MAP }
  case                           { \s -> CASE }
  in                             { \s -> IN }
  for                            { \s -> FOR }
  forall                         { \s -> FORALL }
  exists                         { \s -> EXISTS }
  \+                             { \s -> PLUS }
  \-                             { \s -> MINUS }
  \*                             { \s -> MULT }
  \/                             { \s -> DIV }
  \%                             { \s -> MOD }
  \^                             { \s -> POWER }
  \=\=                           { \s -> EQEQ }
  \!\=                           { \s -> NEQ }
  \<                             { \s -> BLOGLex.LT }
  \>                             { \s -> BLOGLex.GT }
  \!                             { \s -> NOT }
  \<\=                           { \s -> LEQ }
  \>\=                           { \s -> GEQ }
  \&                             { \s -> AND }
  true | false                   { \s -> BOOLEAN_LITERAL (s == "true") }
  $digit+                        { \s -> INT_LITERAL (read s) }
  \-? $digit+ \. $digit+         { \s -> DOUBLE_LITERAL (read s) }
  \' $char \'                    { \s -> CHAR_LITERAL (read s) }
  \" (.)* \"                     { \s -> STRING_LITERAL s }
  @ident                         { \s -> ID s }    

{
-- The token type (see blog-langref.pdf):
data Token
  = SEMI | COMMA | DOT 
  | ID String | LIST | MAP
  | NUMSIGN 
  | DISTRIB | DISTRIBUTION | PARAM | COLON 
  | INT_LITERAL Int | STRING_LITERAL String | CHAR_LITERAL Char | DOUBLE_LITERAL Double | BOOLEAN_LITERAL Bool | NULL 
  | AND | OR | NOT | AT | FORALL | EXISTS | FOR | DOUBLERIGHTARROW
  | IF | THEN | ELSE 
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET | RIGHTARROW 
  | PLUS | MINUS | MULT | DIV | MOD | POWER
  | EQ | EQEQ | NEQ | LT | LEQ | GEQ | GT | IN | CASE
  | OBS | QUERY | TYPE | DISTINCT | FIXED | RANDOM | ORIGIN 
  deriving (Eq, Show)

-- Lexer method (used in the parser, see BLOGParse.y)
lexer :: String -> [Token]
lexer = alexScanTokens

-- main method for testing
main :: IO ()
main = do
    filename <- getLine
    string <- readFile filename
    putStrLn $ (show.lexer) string
}
