%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID 
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF 
%start exp_parser
%type <A1.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
  expr EOF                  {$1}
;

expr: 
    expr DISJ conj_expression          {Disjunction($1,$3)}
    | conj_expression                     {$1}

conj_expression:
    conj_expression CONJ not_expression    { Conjunction($1,$3) } 
    | not_expression                   { $1 }
;   

not_expression:
    NOT not_expression          { Not($2) } 
    | compare_expression                   { $1 }
;

compare_expression:
    compare_expression EQ addsub_expression {Equals($1,$3)}
    | compare_expression GT addsub_expression {GreaterT($1,$3)}
    | compare_expression LT addsub_expression {LessT($1,$3)}
    | compare_expression GT EQ addsub_expression {GreaterTE($1,$4)}
    | compare_expression LT EQ addsub_expression {LessTE($1,$4)}
    | addsub_expression                        { $1 }

addsub_expression:
    addsub_expression MINUS dmr_expression  { Sub($1,$3) }
    | addsub_expression PLUS dmr_expression { Add($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | dmr_expression                  { $1 }
;

dmr_expression:
    dmr_expression REM abs_expression   { Rem($1,$3) }
    |dmr_expression TIMES abs_expression   { Mult($1,$3) }
    |dmr_expression DIV abs_expression   { Div($1,$3) }
    |abs_expression                         { $1 }
; 

abs_expression:
    ABS abs_expression         { Abs($2) } 
    | neg_expression           { $1 }
;

neg_expression:
    TILDA neg_expression         { Negative($2) } 
    | functionn 	 				{$1}
;

functionn:
   functionn LP expr RP              {FunctionCall($1,$3)}
    | BACKSLASH ID DOT ifthenelse        {FunctionAbstraction($2,$4)}
    | ifthenelse              {$1}
;

ifthenelse:
    IF expr THEN expr ELSE expr FI {IfThenElse($2,$4,$6)}
    | project                   {$1}
;

project:
    PROJ LP INT COMMA INT RP project      {Project(($3,$5),$7)} 
    | tuple                     {$1}
;

tuple:
    LP listt RP             {Tuple((List.length($2)),($2))}
    | lett							{$1}
;

listt:
    expr COMMA listt    {[$1]@($3)}
    | expr                    {[$1]}
;

lett:
   LET ID EQ expr IN expr END        {Let($2,$4,$6)}
    | paren_expression              {$1}
;


paren_expression:
    LP expr RP              {InParen($2)}
    | constant              {$1}
;

constant:
    ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | BOOL                             { B($1) }
;

/* Implement the grammar rules for definitions, which may use the parser for expression  */



