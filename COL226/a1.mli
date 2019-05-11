open A0


(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  | Cmp of exptree
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *) 
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
   (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of string * exptree * exptree
  | FunctionAbstraction of string * exptree
  | FunctionCall of exptree * exptree
  



(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of int | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | COND of (opcode list)*(opcode list)
  | TUPLE of int | PROJ of int*int |  BIND of opcode | UNBIND of opcode 
  | CLOS of string * (opcode list) | RET | APP
  (* | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF | REC *)


type answer = Num of int | Bool of bool | Tup of int * (answer list) | Cl of string * table  * (opcode list)

and
  table = (string * answer) list 

type stack = answer list

type dump = (stack*table*(opcode list)) list

type table2 = (string * cl) list 

and 
  (* v= VClosure of answer*table2 

type *) cl = Closure of exptree*table2 (* | *)

val compile: exptree -> opcode list

val secd : stack -> table -> (opcode list) -> dump -> answer

val krivine : cl -> cl list -> cl

val fkrivine : cl -> answer

