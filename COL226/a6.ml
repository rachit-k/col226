exception IllformedStack
exception TypeError
exception Not_found

(* Display
Return(to stack)
Set a var (to avar or const)
Fun call *)

let val_stack =ref([[3;0;0;0]])	(* List.nth *)
let fun_stack =ref(["main"])
;;
(* let fp=0
let sl=0 *)

type func = Head | Record of string * func * (string list) * (string list)
type varr = Var of string| Int of int
;;
let main = Record("main", Head , ["a";"b";"c"], ["a";"b";"c"])
let p = Record("P", main, ["x";"y";"z";"a"], ["a";"b";"c";"x";"y";"z"])
let s = Record("S", p, ["c";"k";"m";"n"], ["a";"b";"c";"x";"y";"z";"k";"m";"n"])
let r = Record("R", p, ["w";"i";"j";"b"], ["a";"b";"c";"x";"y";"z";"w";"i";"j"])
let v = Record("V", r, ["m";"n";"c"], ["a";"b";"c";"x";"y";"z";"w";"i";"j";"m";"n"])
let q = Record("Q", main, ["z";"w";"x";"b"], ["a";"b";"c";"z";"w";"x"])
let t = Record("T", q, ["a";"y";"i";"f"], ["a";"b";"c";"z";"w";"x";"y";"i";"f"])
let u = Record("U", q, ["c";"z";"p";"g"], ["a";"b";"c";"z";"w";"x";"p";"g"])
let w = Record("W", t, ["c";"z";"j";"h"], ["a";"b";"c";"z";"w";"y";"i";"f";"j";"h"])

(* let functions =["main";"P";"S";"R";"V";"Q";"T";"U";"W"] *)



let functions =[p,s,r,v,q,t,u,w]
;;
let getName l = match l with Record(a,b,c,d) -> a
                |_ -> raise TypeError
let getParent l = match l with Record(a,b,c,d) -> b
                |_ -> raise TypeError   
let getLocalVar l = match l with Record(a,b,c,d) -> c
                |_ -> raise TypeError 
let getAccVar l = match l with Record(a,b,c,d) -> d
                |_ -> raise TypeError   
;;
let rec last_element list = 
    match list with 
        [] -> raise IllformedStack
       | [x] -> x
       | x::xs -> last_element xs
;;

let getRecord fn =  if fn="main" then main else if fn="P" then p else if fn="S" then s else if fn="R" then r else if fn="V" then v else if fn="Q" then q else if fn="T" then t else if fn="U" then u else if fn="W" then w  else raise TypeError ;;
;;

let rec get_realfunc f n= if n=0 then f else get_realfunc (getParent f) (n-1)
;;

let rec list_getpos x lst c = match lst with
     [] -> -1
    | hd::xs -> if (hd=x) then c else list_getpos x xs (c+1)   
;;

let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l
;;


let rec set_help var x r= if (List.mem var (getLocalVar r))= true then (let n1 = (list_getpos var (getLocalVar r) 0) in 
replace (List.nth !val_stack (last_element (List.hd(!val_stack)))) n1 x)
 						else set_help var x (getParent r) 
;;
let rec set_help2 var r= if (List.mem var (getLocalVar r))= true then (let n1 = (list_getpos var (getLocalVar r) 0) in 
																		(List.nth (List.nth !val_stack (last_element (List.hd(!val_stack)))) n1) )
 						else set_help2 var (getParent r) 						
;;

let set var x = let r = getRecord (List.hd(!fun_stack)) in 
				(if (List.mem var (getAccVar r))=false then raise Not_found 
				else (match x with 
					Int(x1)-> val_stack := ( replace !val_stack (last_element (List.hd(!val_stack))) (set_help var x1 r) )
					| Var(var1) -> let x1 = (set_help2 var1 r ) in 
					val_stack := ( replace !val_stack (last_element (List.hd(!val_stack))) (set_help var x1 r) )
					))
;;

let display_fun() =  List.hd(!fun_stack)
;;
let display_acc() = getAccVar (getRecord (List.hd(!fun_stack)))
;;

let rec firstk k xs = match xs with
| [] -> failwith "firstk"
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs
;;

let  rec displayval_help r l= let val_stack2 = (firstk (List.length((List.hd(!val_stack)))-1) (List.hd(!val_stack))) in ( if (getName r) = "main" then (l@(val_stack2) )
else displayval_help (getParent r) (l@(val_stack2)) )
;;

let display_val() = (displayval_help (getRecord (List.hd(!fun_stack))) [])
;;

let return() = if ((List.length(!fun_stack))=0) then raise IllformedStack else (val_stack := (List.tl(!val_stack)) ; fun_stack := (List.tl(!fun_stack)))
;; 



(* let display = 

let return() =  
 *)
