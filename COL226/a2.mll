{
  open A3
  exception Invalidtoken of char
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  - This is sample implementation. Please rewrite them as per the specifications
*)

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let nzdigit =['1'-'9']
let letter = ['a'-'z' 'A'-'Z'] | digit | '_' |'''
let capletter =['A'-'Z']
let integer = ((nzdigit(digit*))|'0')    (*Integer constants*)
let boolean= 'T'|'F'
let abss =  "abs"   
let plus = "+"
let minus= "-"
let mul= "*"
let divv= "div"
let modd= "mod"
let exp= "^"
let lp= "("
let rp=")"
let tilda="~"
let nott="not"
let andd="/\\"
let orr="\\/"
let eq="="
let gta=">"
let lta="<"
let geq=">="
let leq="<="
let iff="if"
let thenn="then"
let elsee="else" 
let fii="fi" 
let lp="("
let rp=")"    
let comma=","           
let projj="proj"      
let id = capletter (letter*)              (*Identifiers*)
let deff= "def"                           (*definition construct*)



(*rules section*)
rule read = parse   
 abss                {ABS}    
| plus                {PLUS}    
| minus               {MINUS}    
| mul                 {TIMES}    
| divv                {DIV}    
| modd                {REM}    
| tilda               {TILDA}    
| lp                  {LP}  
| rp                  {RP}  
| nott                {NOT}
| andd                {CONJ}
| orr                 {DISJ}
| eq                  {EQ}
| gta                 {GT}
| lta                 {LT}
| lp                  {LP}
| rp                  {RP}
| iff                 {IF}
| thenn               {THEN}
| elsee               {ELSE}
| fii                 {FI}
| comma               {COMMA}
| projj               {PROJ}
| deff                {DEF}
| "let"				  {LET}
| "in"				  {IN}
| "end"				  {END}
| "\\"				  {BACKSLASH}
| "."				  {DOT}
| "def"				  {DEF}
| ";"				  {SEMICOLON}
| "||"				  {PARALLEL}
| "local"			  {LOCAL}
| boolean as n        {if (n='T') then BOOL(true) else BOOL(false) }
| integer as n        {if ((String.get n 0) = '+' ) then (INT(int_of_string (String.sub n 1 ((String.length n)-1))))
                      else (INT(int_of_string n))}   (* so that positive signed integers may be taken as an input (else the function int_of_string throws an error in older compilers)*)
| id as s             {ID(s)}
| whitespace          {read lexbuf}      
| eof                 {EOF}
| _                   {raise (Invalidtoken(Lexing.lexeme_char lexbuf 0))}

