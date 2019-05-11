exception Not_implemented
exception IllformedStack
exception Varnotfound

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
  
;;


type opcode = VAR of string | NCONST of int | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | COND of (opcode list)*(opcode list)
  | TUPLE of int | PROJ of int*int |  BIND of opcode | UNBIND of opcode 
  | CLOS of string * (opcode list) | RET | APP
  (* | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF | REC *)
;;

type answer = Num of int | Bool of bool | Tup of int * (answer list) | Cl of string * table  * (opcode list)(* exptree*table *)

and
  table= (string * answer) list 
;;
 
type stack= answer list
;;

type dump = (stack*table*(opcode list)) list
;;  

type table2 = (string * cl) list 

and 
  (* v= VClosure of answer*table2 

type *) cl = Closure of exptree*table2
;;

let rec compile t = match t with
 N(i)->[NCONST(i)]
 | B(i)-> [BCONST(i)]
 | Var(i)-> [VAR(i)]
 | FunctionAbstraction(x,t1) -> [CLOS(x, (compile t1)@[RET])]  
 | FunctionCall(t1, t2) -> (compile t1)@(compile t2)@[APP]
 | InParen(t1) -> (compile(t1))@[PAREN]
 | Add(t1,t2) -> (compile(t1))@(compile(t2))@[PLUS]
 | Mult(t1,t2) -> (compile(t1))@(compile(t2))@[MULT]
 | Sub(t1,t2) -> (compile(t2))@(compile(t1))@[MINUS]
 | Div(t1,t2) -> (compile(t2))@(compile(t1))@[DIV]
 | Rem(t1,t2) -> (compile(t2))@(compile(t1))@[REM]
 | Abs(t1) -> (compile(t1))@[ABS]
 | Negative(t1) -> (compile(t1))@[UNARYMINUS]
 | Not(t1) -> (compile(t1))@[NOT]
 | Equals(t1,t2) -> (compile(t2))@(compile(t1))@[EQS]
 | GreaterTE(t1,t2) -> (compile(t2))@(compile(t1))@[GTE]
 | LessTE(t1,t2) -> (compile(t2))@(compile(t1))@[LTE]
 | GreaterT(t1,t2) -> (compile(t2))@(compile(t1))@[GT]
 | LessT(t1,t2) -> (compile(t2))@(compile(t1))@[LT]
 | Cmp(t1) -> [NCONST(0)]@(compile t1)@[EQS]    
 | IfThenElse(t1,t2,t3) -> (compile(t1))@[COND(compile t2, compile t3)]
 | Conjunction(t1,t2) -> (compile(t1))@(compile(t2))@[CONJ]
 | Disjunction(t1,t2) -> (compile(t1))@(compile(t2))@[DISJ]
 | Tuple(i,l) -> (match l with x::xs -> (if List.length(l)=1 then (compile(x))@[TUPLE(i)]
                                              else (compile(x))@(compile(Tuple(i,xs))))
                                 |_-> raise IllformedStack)
 | Project((i,n),t1) -> compile(t1)@[PROJ(i,n)] 
 | Let(x,t1,t2) -> (compile t1)@[BIND(VAR(x))]@(compile t2)@[UNBIND(VAR(x))]
;;


let rec find e x= match e with 
  [] -> raise Varnotfound
  | (i,a)::xs-> if x=i then a else find xs x
;;

let rec mapp fn l fn1 = match l with 
[] -> []
| x::xs -> (fn x fn1)::(mapp fn xs fn1)
;;

let rec getith l i = match l with 
    []-> raise IllformedStack
    |x::xs -> (if i=1 then x else (getith xs (i-1)) )
;;

let rec mktup l a i = match a with
  []-> if(i=0) then l else raise IllformedStack
  |x::xs->if(i>0) then mktup (x::l) xs (i-1) else l
;;

let rec drop a i =
   if (i=0) then a else (drop (List.tl(a)) (i-1));;

let rec unb l x e= match e with 
                      []-> l
                      | (y,a)::ee-> (if x=y then l@ee else unb ((y,a)::l) x ee)
;;

let rec secd stk envt code dump = match (stk, envt, code, dump) with
  (x::s, _, [], _) -> x
  | (s, e, NCONST(i)::c, d) -> (secd (Num(i)::s) e c d)
  | (s, e, BCONST(i)::c, d) -> (secd (Bool(i)::s) e c d)
  | (s, e, VAR(x)::c, d) -> (secd ((find e x)::s) e c d)
  | (s, e, CLOS(x, cc)::c, d) -> (secd (Cl(x, e, cc)::s) e c d)
  | (x::Cl(xx, ee, cc)::s, e, APP::c, d) -> (secd [] ((xx, x)::ee) cc ((s, e, c)::d))
  | (x::s, e, RET::c, (ss, ee, cc)::d) -> (secd (x::ss) ee cc d)
  | (s, e, PAREN::c, d) -> (secd s e c d)
  | (Num(i)::Num(j)::s, e, PLUS::c, d) -> (secd (Num(i+j)::s) e c d)
  | (Num(i)::Num(j)::s, e, MINUS::c, d) -> (secd (Num(i-j)::s) e c d)
  | (Num(i)::Num(j)::s, e, MULT::c, d) -> (secd (Num(i*j)::s) e c d)
  | (Num(i)::Num(j)::s, e, DIV::c, d) -> (secd (Num(i/j)::s) e c d)
  | (Num(i)::Num(j)::s, e, REM::c, d) -> (secd (Num(i mod(j))::s) e c d)
  | (Num(i)::s, e, ABS::c, d) -> (secd (Num(if i>0 then i else ((-1)*i))::s) e c d)
  | (Num(i)::s, e, UNARYMINUS::c, d) -> (secd (Num((-1)*i)::s) e c d)
  | (Num(i)::Num(j)::s, e, EQS::c, d) -> (secd (Bool(if i==j then true else false)::s) e c d)
  | (Num(i)::Num(j)::s, e, GTE::c, d) -> (secd (Bool(if i>=j then true else false)::s) e c d)
  | (Num(i)::Num(j)::s, e, LTE::c, d) -> (secd (Bool(if i<=j then true else false)::s) e c d)
  | (Num(i)::Num(j)::s, e, GT::c, d) -> (secd (Bool(if i>j then true else false)::s) e c d)
  | (Num(i)::Num(j)::s, e, LT::c, d) -> (secd (Bool(if i<j then true else false)::s) e c d)
  | (Bool(i)::s, e, COND(c1, c2)::c, d) -> if (i=true) then (secd s e (c1@c) d) else (secd s e (c2@c) d)
  | (Bool(i)::Bool(j)::s, e, CONJ::c, d) -> (secd (Bool(i&&j)::s) e c d)
  | (Bool(i)::Bool(j)::s, e, DISJ::c, d) -> (secd (Bool(i||j)::s) e c d)
  | (Bool(i)::s, e, NOT::c, d) -> (secd (Bool(not i)::s) e c d)
  | (s, e, TUPLE(i)::c, d) -> secd (Tup(i,(mktup [] s i))::(drop s i)) e c d
  | (Tup(n,li)::s, e, PROJ(i,nn)::c, d) -> if ((n==nn)&&(n==List.length(li))) then (secd ((List.nth li (i-1))::s) e c d) else raise IllformedStack
  | (s'::s, e, BIND(VAR(x))::c, d) -> secd s ((x,s')::e) c d
  | (s, e, UNBIND(VAR(x))::c, d) -> secd s (unb [] x e) c d
  | _ -> raise IllformedStack
;;


let addk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(N(i+j),[])
                  |_-> raise IllformedStack
;;
let subk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(N(i-j),[])
                  |_-> raise IllformedStack
;;
let multk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(N(i*j),[])
                  |_-> raise IllformedStack
;;
let divk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(N(i/j),[])
                  |_-> raise IllformedStack
;;
let remk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(N(i mod(j)),[])
                  |_-> raise IllformedStack
;;
let absk c1 = match c1 with Closure(N(i),e1)->Closure(N(if i>0 then i else ((-1)*i)),[])
                  |_-> raise IllformedStack
;;
let negk c1 = match c1 with Closure(N(i),e1)->Closure(N((-1)*i),[])
                  |_-> raise IllformedStack
;;
let eqsk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(B(if i==j then true else false),[])
                  |_-> raise IllformedStack
;;
let gtek c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(B(if i>=j then true else false),[])
                  |_-> raise IllformedStack
;;
let ltek c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(B(if i<=j then true else false),[])
                  |_-> raise IllformedStack
;;
let gtk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(B(if i>j then true else false),[])
                  |_-> raise IllformedStack
;;
let ltk c1 c2= match (c1,c2) with (Closure(N(i),e1), Closure(N(j),e2)) -> Closure(B(if i<j then true else false),[])
                  |_-> raise IllformedStack
;;
let andk c1 c2= match (c1,c2) with (Closure(B(i),e1), Closure(B(j),e2)) -> Closure(B(i&&j),[])
                  |_-> raise IllformedStack
;;
let ork c1 c2= match (c1,c2) with (Closure(B(i),e1), Closure(B(j),e2)) -> Closure(B(i||j),[])
                  |_-> raise IllformedStack
;;
let notk c1 = match c1 with Closure(B(i),e1)->Closure(B(not i),[])
                  |_-> raise IllformedStack
;;

let letk c1 x =match c1 with 
                Closure(t, e) -> Closure(t, unb [] x e) 
;;
 
 let rec findk e x= match e with 
  [] -> raise Varnotfound
  | (i,c)::xs-> if x=i then c else findk xs x
;;  

let rec krivine clos stk = match clos with
  Closure(N(i),e)-> Closure(N(i),e)
  | Closure(B(i),e)-> Closure(B(i),e)
  | Closure(Var(x),e)-> krivine (findk e x) stk    
  | Closure(FunctionAbstraction(x,t1),e) -> (match stk with s::sx -> (krivine (Closure(t1,(x,s)::e)) sx )|_-> raise IllformedStack)
  | Closure(FunctionCall(t1,t2),e)-> krivine (Closure(t1,e)) ((Closure(t2,e))::stk)
  | Closure(InParen(t1),e)-> krivine (Closure(t1,e)) stk
  | Closure(Add(t1,t2),e)-> krivine (addk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Sub(t1,t2),e)-> krivine (subk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Mult(t1,t2),e)-> krivine (multk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Div(t1,t2),e)-> krivine (divk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Rem(t1,t2),e)-> krivine (remk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Abs(t1),e)-> krivine (absk (krivine (Closure(t1,e)) []) ) stk
  | Closure(Negative(t1),e)-> krivine (negk (krivine (Closure(t1,e)) []) ) stk
  | Closure(Equals(t1,t2),e)-> krivine (eqsk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(GreaterTE(t1,t2),e)-> krivine (gtek (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(LessTE(t1,t2),e)-> krivine (ltek (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(GreaterT(t1,t2),e)-> krivine (gtk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(LessT(t1,t2),e)-> krivine (ltk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Cmp(t1),e)-> krivine (eqsk (krivine (Closure(t1,e)) [])  (krivine (Closure(N(0),e)) [])) stk
  | Closure(Conjunction(t1,t2),e)-> krivine (andk (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Disjunction(t1,t2),e)-> krivine (ork (krivine (Closure(t1,e)) [])  (krivine (Closure(t2,e)) [])) stk
  | Closure(Not(t1),e)-> krivine (notk (krivine (Closure(t1,e)) []) ) stk 
  | Closure(Let(x,t1,t2),e)-> letk (krivine (Closure(t2, (x, (krivine (Closure(t1,e)) []))::e)) stk) x 
  | Closure(Project((i,n), t1),e)-> (match t1 with Tuple(nn,l)-> if ((n==List.length(l))&&(n==nn)) then (krivine (Closure((List.nth l (i-1)),e)) stk) else raise IllformedStack|_-> raise IllformedStack )  (* not sure *) 
  | Closure(IfThenElse(t1,t2,t3),e)-> (match (krivine (Closure(t1,e)) []) with 
                                      Closure(B(i),e) -> if i==true then (krivine (Closure(t2,e)) stk) else (krivine (Closure(t3,e)) stk)
                                      |_-> raise IllformedStack) 
  | _-> raise Not_implemented                                  
;;

let rec fkrivine clos= match clos with
  Closure(N(i),e)-> Num(i)
  |Closure(B(i),e)-> Bool(i)
  |_-> raise Not_implemented
;;
(* 
let p1 = FunctionCall (FunctionAbstraction ("x", Mult (N 3, Var "x")), N 4);;
let p2 = IfThenElse(Cmp (N 7),FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 31), N 0);;
let p3 = IfThenElse(Cmp (N 0),FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 4),N 110);; 
let p4 = IfThenElse(Cmp (N 7),FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 31), Let ("y", (N 3), (Add (N 3, Var "y"))) );;
let p5 = Project((2,4), Tuple(4,[N 1; FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 4); N 4; B true]));;
 *)

let p1 =  FunctionCall (FunctionAbstraction ("x", Mult (N 3, Var "x")), N 4);;
  (*12*)
let p2 = IfThenElse
   (GreaterT((N 7),(N 0)),
    FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 31), 
    N 0);;
   (*34*)
let p3 = IfThenElse
    (GreaterT((N 0),(N 0)),
    FunctionCall (FunctionAbstraction ("x", Add (N 3, Var "x")), N 4),
        N 110);;
    (*110*)

let p4 = FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Conjunction(Var "x", Var "y")), B true)), B false);;
(*false*)

let p5 = FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Disjunction(Var "x", Var "y")), B true)), B false);;
(*true*)

let p6 = FunctionCall(FunctionAbstraction("x", Mult(Var "x", FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Add(Var "x", Var "y")), N 4)), N 3))), N 2);;
(*14*)

let p7 = IfThenElse(GreaterT((FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction( "y", Add(Var "x", Var "y")), N 4)), N (-5))),(N 0)), N (-29), FunctionCall(FunctionAbstraction("x", Add(Var "x", 
  FunctionCall(FunctionAbstraction("x", Add(Var "x", N 1)), N 7))), N 5));;
(*13*)

let p8 = FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Add(Var "x", Var "y")), N 4)), FunctionCall(FunctionAbstraction("x", Mult(Var "x", N 2)), N 3));;
(*10*)

let p9 = FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Mult(Var "x", Var "y")), Var "x")), N 4);;
(*16*)

let p10 = FunctionCall(FunctionAbstraction("x", Add(Var "x", FunctionCall(FunctionAbstraction("x", Mult(Var "x", N 2)), FunctionCall(FunctionAbstraction("x", Add(Var "x", N 4)), N 3)))), N 20);;
(*34*)

let p11 = FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Conjunction(Var "x", Var "y")), Var "x")), B true);;
(*true*)

let p12 = IfThenElse(GreaterT((FunctionCall(FunctionAbstraction("x", Mult(Var "x", N 2)), N 4)),(N 0)), FunctionCall(FunctionAbstraction("x", FunctionCall(FunctionAbstraction("y", Disjunction(Var "x", Var "y")), Var "x")), B false), B true);;
(*false*)

let p13 = FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", B true)), FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", B true)), B true)))), B true);;
(*true*)

let p14 = FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", B true)), FunctionCall(FunctionAbstraction("x", Conjunction(Var "x", B true)), B true)))), B false);;
(*false*)

let p15 = IfThenElse(GreaterT((FunctionCall(FunctionAbstraction("x", Mult(Var "x", FunctionCall(FunctionAbstraction("y", Var "y"), Var "x"))), N 1)),(N 0)), FunctionCall(FunctionAbstraction("x", Add(Var "x", FunctionCall(FunctionAbstraction("x", Add(Var "x", N 1)), N 3))), N 5), N (-1));;
 
let check_secd n inp out = let ans = (secd [] [] inp []) in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = fkrivine inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out) 
    then print_string("Passed ") 
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 (compile p1) (Num 12);;
check_secd 2 (compile p2) (Num 34);;
check_secd 3 (compile p3) (Num 110);;
check_secd 4 (compile p4) (Bool false);;
check_secd 5 (compile p5) (Bool true);;
check_secd 6 (compile p6) (Num 14);;
check_secd 7 (compile p7) (Num 13);;
check_secd 8 (compile p8) (Num 10);;
check_secd 9 (compile p9) (Num 16);;
check_secd 10 (compile p10) (Num 34);;
check_secd 11 (compile p11) (Bool true);;
check_secd 12 (compile p12) (Bool false);;
check_secd 13 (compile p13) (Bool true);;
check_secd 14 (compile p14) (Bool false);;
check_secd 15 (compile p15) (Num 9);;  

print_heading "Krivine test cases";;

check_krivine 1 (krivine (Closure(p1,[])) []) (Num 12);;
check_krivine 2 (krivine (Closure( p2,[])) []) (Num 34);;
check_krivine 3 (krivine (Closure( p3,[])) []) (Num 110);;
check_krivine 4 (krivine (Closure( p4,[])) []) (Bool false);;
check_krivine 5 (krivine (Closure( p5,[])) []) (Bool true);;
check_krivine 6 (krivine (Closure( p6,[])) []) (Num 14);;
check_krivine 7 (krivine (Closure( p7,[])) []) (Num 13);;
check_krivine 8 (krivine (Closure( p8,[])) []) (Num 10);;
check_krivine 9 (krivine (Closure( p9,[])) []) (Num 16);;
check_krivine 10 (krivine (Closure( p10,[])) []) (Num 34);;
check_krivine 11 (krivine (Closure( p11,[])) []) (Bool true);;
check_krivine 12 (krivine (Closure( p12,[])) []) (Bool false);;
check_krivine 13 (krivine (Closure( p13,[])) []) (Bool true);;
check_krivine 14 (krivine (Closure( p14,[])) []) (Bool false);;
check_krivine 15 (krivine (Closure( p15,[])) []) (Num 9);;  
(* 
# secd [] [] (compile p5) [];;
- : answer = Num 3
# fkrivine (krivine (Closure(p5,[])) []);;
- : answer = Num 3 *)

