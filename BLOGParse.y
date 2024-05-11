{
module BLOGParse (parser, Program(..), Statement(..), Declaration(..), Expr(..), Type(..), main) where
import BLOGLex (lexer, Token(..))
import Prelude hiding (GT, LT, EQ)  -- clashes with Token type
}

%name parser
%tokentype { Token }
%error { parseError }

%left UMINUS

%token
      SEMI             { BLOGLex.SEMI }
      COMMA            { BLOGLex.COMMA }
      DOT              { BLOGLex.DOT }
      ID               { BLOGLex.ID $$ }
      LIST             { BLOGLex.LIST }
      MAP              { BLOGLex.MAP }
      NUMSIGN          { BLOGLex.NUMSIGN }
      DISTRIB          { BLOGLex.DISTRIB }            {- the tilde symbol ~ -}
      DISTRIBUTION     { BLOGLex.DISTRIBUTION }
      PARAM            { BLOGLex.PARAM }
      COLON            { BLOGLex.COLON }
      INT_LITERAL      { BLOGLex.INT_LITERAL $$ }
      STRING_LITERAL   { BLOGLex.STRING_LITERAL $$ }
      CHAR_LITERAL     { BLOGLex.CHAR_LITERAL $$ }
      DOUBLE_LITERAL   { BLOGLex.DOUBLE_LITERAL $$ }
      BOOLEAN_LITERAL  { BLOGLex.BOOLEAN_LITERAL $$ }
      NULL             { BLOGLex.NULL }
      AND              { BLOGLex.AND }
      OR               { BLOGLex.OR }
      NOT              { BLOGLex.NOT }
      AT               { BLOGLex.AT }
      FORALL           { BLOGLex.FORALL }
      EXISTS           { BLOGLex.EXISTS }
      FOR              { BLOGLex.FOR }
      DOUBLERIGHTARROW { BLOGLex.DOUBLERIGHTARROW }
      IF               { BLOGLex.IF }
      THEN             { BLOGLex.THEN }
      ELSE             { BLOGLex.ELSE }
      CASE             { BLOGLex.CASE }
      IN               { BLOGLex.IN }
      LPAREN           { BLOGLex.LPAREN }
      RPAREN           { BLOGLex.RPAREN }
      LBRACE           { BLOGLex.LBRACE }
      RBRACE           { BLOGLex.RBRACE }
      LBRACKET         { BLOGLex.LBRACKET }
      RBRACKET         { BLOGLex.RBRACKET }
      RIGHTARROW       { BLOGLex.RIGHTARROW }
      PLUS             { BLOGLex.PLUS }
      MINUS            { BLOGLex.MINUS }
      MULT             { BLOGLex.MULT }
      DIV              { BLOGLex.DIV }
      MOD              { BLOGLex.MOD }
      POWER            { BLOGLex.POWER }
      EQ               { BLOGLex.EQ }
      EQEQ             { BLOGLex.EQEQ }
      NEQ              { BLOGLex.NEQ }
      LT               { BLOGLex.LT }
      LEQ              { BLOGLex.LEQ }
      GEQ              { BLOGLex.GEQ }
      GT               { BLOGLex.GT }
      OBS              { BLOGLex.OBS }
      QUERY            { BLOGLex.QUERY }
      TYPE             { BLOGLex.TYPE }
      DISTINCT         { BLOGLex.DISTINCT }
      FIXED            { BLOGLex.FIXED }
      RANDOM           { BLOGLex.RANDOM }
      ORIGIN           { BLOGLex.ORIGIN }      

%%

program :
    opt_statement_lst { $1 }

opt_statement_lst :
    {- empty -}     { [] }
    | statement_lst { $1 }

statement_lst :
    statement SEMI statement_lst { $1 : $3 }
    | statement statement_lst    { $1 : $2 }
    | statement SEMI             { [$1] }
    | statement                  { [$1] }

statement :
    declaration_stmt { DECSTMT $1 }
    | evidence_stmt  { EVDSTMT $1 }
    | query_stmt     { QRYSTMT $1 }

declaration_stmt :
    type_decl           { $1 }
    | fixed_func_decl   { $1 }
    | rand_func_decl    { $1 }
    | origin_func_decl  { $1 }
    | number_stmt       { $1 }
    | distinct_decl     { $1 }
    | parameter_decl    { $1 }
    | distribution_decl { $1 }

type_decl : 
    TYPE ID { TYPDECL $2 }

type :
    refer_name   { (SIMPLETYPE $1) }
    | list_type  { (LISTTYPE $1) }
    | array_type { (ARRAYTYPE $1) }
    | map_type   { (MAPTYPE $1) }

type_type : type { $1 }  {- added to repair the grammar -}

list_type :
    LIST LT refer_name GT { $3 }

array_type_or_sub :
    refer_name LBRACKET { ($1, 1) }

array_type :
    array_type_or_sub RBRACKET     { $1 }
    | array_type LBRACKET RBRACKET { let (str,n) = $1 in (str,n+1)  }

map_type :
    MAP LT type COMMA type GT { ($3, $5) }

opt_parenthesized_type_var_lst :
    {- empty -}                  { [] }
    | parenthesized_type_var_lst { $1 }
    | type_var_lst               { $1 }

parenthesized_type_var_lst :
    LPAREN RPAREN                { [] }
    | LPAREN type_var_lst RPAREN { $2 }

extra_commas :
    COMMA COMMA          { }
    | extra_commas COMMA { }

type_var_lst :
    type ID COMMA type_var_lst          { ($1,$2) : $4 }
    | type ID                           { [($1, $2)] }
    | type ID extra_commas type_var_lst { ($1,$2) : $4 }
    | type ID type_var_lst              { ($1,$2) : $3 }
    | type COMMA type_var_lst           { ($1,"") : $3 } 

fixed_func_decl :
    FIXED type_type ID opt_parenthesized_type_var_lst EQ expression  { FFUDECL $2 $3 $4 $6 }

rand_func_decl :
    RANDOM type_type ID opt_parenthesized_type_var_lst dependency_statement_body { RFUDECL $2 $3 $4 $5 }

number_stmt :
    NUMSIGN refer_name opt_parenthesized_origin_var_list dependency_statement_body { NUMDECL $2 $3 $4 }
    | NUMSIGN opt_parenthesized_origin_var_list dependency_statement_body          { NUMDECL "" $2 $3 }  

opt_parenthesized_origin_var_list :
    {- empty -}                     { [] }
    | LPAREN origin_var_list RPAREN { $2 }

origin_var_list :
    ID EQ ID COMMA origin_var_list          { ($1,$3) : $5 }
    | ID EQ ID extra_commas origin_var_list { ($1,$3) : $5 }
    | ID EQ COMMA origin_var_list           { ($1,"") : $4 }  
    | ID EQ ID origin_var_list              { ($1,$3) : $4 }
    | ID EQ ID                              { [($1,$3)] }
    | ID ID                                 { [($1,$2)] }

origin_func_decl :
    ORIGIN type_type ID LPAREN type_type RPAREN { OFUDECL ($2,$3) $5 }
    | ORIGIN type_type LPAREN type_type RPAREN  { OFUDECL ($2,"") $4 }
    | ORIGIN type_type ID LPAREN type_type      { OFUDECL ($2,$3) $5 }
    | ORIGIN type_type ID type_type RPAREN      { OFUDECL ($2,$3) $4 }

distinct_decl :
    DISTINCT refer_name id_or_subid_list { DNTDECL $2 $3 }

id_or_subid_list :
    id_or_subid                                  { [$1] }
    | id_or_subid COMMA id_or_subid_list         { $1 : $3 }
    | id_or_subid id_or_subid_list               { $1 : $2 }
    | id_or_subid extra_commas id_or_subid_list  { $1 : $3 }

id_or_subid :
    ID                                  { ($1, -1) }       {- index -1 corresponds to no indexing -}
    | ID LBRACKET INT_LITERAL RBRACKET  { ($1, $3) }

distribution_decl :
    DISTRIBUTION ID EQ refer_name LPAREN opt_expression_list RPAREN { DSTDECL $2 $4 $6 }

refer_name :
    ID                  { $1 }
    | ID DOT refer_name { $1 ++ "." ++ $3 }

dependency_statement_body :
    DISTRIB expression { $2 }

parameter_decl :
    PARAM type ID                     { PRMDECL $2 $3 }
    | PARAM type ID COLON expression  { PRMDECL $2 $3 }

expression :
    operation_expr             { $1 }
    | literal                  { $1 }
    | function_call            { $1 }
    | list_expr                { $1 }
    | map_construct_expression { $1 }
    | quantified_formula       { $1 }
    | set_expr                 { $1 }
    | number_expr              { EXPPLACEHOLD }
    | if_expr                  { $1 }
    | case_expr                { $1 }

literal :
    STRING_LITERAL    { STRING $1 }
    | CHAR_LITERAL    { CHAR $1 }
    | INT_LITERAL     { INT $1 }
    | DOUBLE_LITERAL  { DOUBLE $1 }
    | BOOLEAN_LITERAL { BOOL $1 }
    | NULL            { BLOGParse.NULL }

operation_expr :
    expression PLUS expression                 { BLOGParse.PLUS $1 $3 }
    | expression MINUS expression              { BLOGParse.MINUS $1 $3 }
    | expression MULT expression               { BLOGParse.MULT $1 $3 }
    | expression DIV expression                { BLOGParse.DIV $1 $3 }
    | expression MOD expression                { BLOGParse.MOD $1 $3 }
    | expression POWER expression              { BLOGParse.POWER $1 $3 }
    | expression LT expression                 { BLOGParse.LT $1 $3 }
    | expression GT expression                 { BLOGParse.GT $1 $3 }
    | expression LEQ expression                { BLOGParse.LEQ $1 $3 }
    | expression GEQ expression                { BLOGParse.GEQ $1 $3 }
    | expression EQEQ expression               { BLOGParse.EQEQ $1 $3 }
    | expression NEQ expression                { BLOGParse.NEQ $1 $3 }
    | expression AND expression                { BLOGParse.AND $1 $3 }
    | expression OR expression                 { BLOGParse.OR $1 $3 }
    | expression DOUBLERIGHTARROW expression   { BLOGParse.IMPLIES $1 $3 }
    | expression LBRACKET expression RBRACKET  { BLOGParse.APPLY $1 $3 }
    | unary_operation_expr                     { $1 }

unary_operation_expr :
    MINUS expression %prec UMINUS  { NEG $2 }
    | NOT expression               { BLOGParse.NOT $2 }
    | AT expression                { BLOGParse.AT $2 }
    | LPAREN expression RPAREN     { $2 }

quantified_formula :
    FORALL type ID expression    { BLOGParse.FORALL $2 $3 $4 }
    | EXISTS type ID expression  { BLOGParse.EXISTS $2 $3 $4 }

function_call :
    refer_name LPAREN opt_expression_list RPAREN  { CALL $1 $3 }
    | refer_name                                  { CALL $1 [] }

if_expr :
    IF expression THEN expression ELSE expression  { IFELSE $2 $4 $6 }
    | IF expression THEN expression                { IFTHEN $2 $4 }

case_expr :
    CASE expression IN map_construct_expression { BLOGParse.CASE $2 $4 }

opt_expression_list :
    expression_list   { $1 }
    | {- empty -}     { [] }

expression_list :
    expression COMMA expression_list            { $1 : $3 }
    | expression                                { [$1] }
    | expression extra_commas expression_list   { $1 : $3 }

semi_colon_separated_expression_list :
    semi_ending_expression_list semi_colon_separated_expression_list  { $1 ++ $2 } 
    | semi_ending_expression_list expression_list                     { $1 ++ $2 }

semi_ending_expression_list :
    expression_list SEMI                { $1 }
    | semi_ending_expression_list SEMI  { $1 }

map_construct_expression :
    LBRACE expression_pair_list RBRACE  { MAPCONSTRUCT $2 }

expression_pair_list :
    expression RIGHTARROW expression COMMA expression_pair_list  { ($1,$3) : $5 }
    | expression RIGHTARROW expression                           { [($1,$3)] }

number_expr :
    NUMSIGN set_expr  { NAN }
    | NUMSIGN type    { NAN }

list_expr :
    LBRACKET opt_expression_list RBRACKET                     { BLOGParse.LIST $2 }
    | LBRACKET semi_colon_separated_expression_list RBRACKET  { BLOGParse.LIST $2 }
    | LBRACKET comprehension_expr RBRACKET                    { $2 }

set_expr :
    explicit_set  { SET $1 }
    | tuple_set   { $1 }

explicit_set :
    LBRACE opt_expression_list RBRACE   { $2 }

comprehension_expr :
    expression_list FOR type_var_lst opt_colon_expr   { COMPREHENSION $1 $3 $4 }

opt_colon_expr :
    {- empty -}          { BOOL True }   {- an empty guard of a comprehension is always true -}
    | COLON expression   { $2 }

tuple_set :
    LBRACE comprehension_expr RBRACE   { $2 }

evidence_stmt :
    OBS evidence   { $2 }

evidence :
    value_evidence  { $1 }

value_evidence :
    expression EQ expression   { ($1, $3) }

query_stmt :
    QUERY expression { $2 }

{
parseError :: [Token] -> a
parseError _ = error "(parse errors aren't descriptive)"

-- pretty printing for Programs
pretty :: Program -> String
pretty = (concat . map ((++";\n").show))

-- main method for testing
main :: IO ()
main = do
    filename <- getLine
    string <- readFile filename
    putStrLn $ (show.parser.lexer) string

type Program = [Statement]

data NOTIMPLEMENTED = NAN

data Statement
    = DECSTMT Declaration
    | EVDSTMT (Expr, Expr)   -- meaning "observe these two are equal"
    | QRYSTMT Expr           -- meaning "query the value of this expression"
    deriving (Show,Eq)

data Declaration
    = DECPLACEHOLD
    | TYPDECL String
    | PRMDECL Type String
    | FFUDECL Type String [(Type,String)] Expr
    | RFUDECL Type String [(Type,String)] Expr
    | DSTDECL String String [Expr]
    | OFUDECL (Type, String) Type
    | NUMDECL String [(String,String)] Expr
    | DNTDECL String [(String,Int)]
    deriving (Show,Eq)

data Type
    = SIMPLETYPE String
    | LISTTYPE String          -- field type
    | ARRAYTYPE (String, Int)  -- field type, dimensionality
    | MAPTYPE (Type, Type)     -- from type, to type
    deriving (Show, Eq)

data Expr
    = EXPPLACEHOLD
    | INT Int
    | STRING String
    | CHAR Char
    | DOUBLE Double
    | BOOL Bool
    | NULL
    | ID String
    | PLUS Expr Expr
    | MINUS Expr Expr
    | MULT Expr Expr
    | DIV Expr Expr
    | MOD Expr Expr
    | POWER Expr Expr
    | LT Expr Expr
    | GT Expr Expr
    | LEQ Expr Expr
    | GEQ Expr Expr
    | EQEQ Expr Expr
    | NEQ Expr Expr
    | AND Expr Expr
    | OR Expr Expr
    | IMPLIES Expr Expr
    | APPLY Expr Expr
    | NEG Expr
    | NOT Expr
    | AT Expr
    | IFELSE Expr Expr Expr
    | IFTHEN Expr Expr
    | CALL String [Expr]
    | MAPCONSTRUCT [(Expr,Expr)]
    | SET [Expr]
    | CASE Expr Expr
    | COMPREHENSION [Expr] [(Type,String)] Expr
    | LIST [Expr]
    | EXISTS Type String Expr
    | FORALL Type String Expr
    deriving (Show,Eq)

}
