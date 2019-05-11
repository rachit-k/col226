open A1
exception Not_implemented
exception IllformedStack
exception Varnotfound
exception TypeException
exception DefException

let rec get_vartype g v = match g with
		[]-> raise Varnotfound
		|x::xs -> if (fst x)=v then (snd x) else (get_vartype xs v)
;;

let rec find_vartype g v= match g with
		[]-> false
		|x::xs -> if (fst x)=v then true else (get_vartype xs v)
;;

let rec common_var l1 l2= match (l1,l2) with
		([],[])->false
		| ([],x)-> false
		| (x,[])-> false
		| (x::xs,y)-> if (find_vartype y (fst x)) then true else (common_var xs y)
;;	
	

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)

let rec get_type g e = match e with
	N(i)->Tint
	 | B(i)->Tbool
	 | Var(i)-> (get_vartype g i)
	 | InParen(t1) -> (get_type g t1)
	 | Add(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tint else raise TypeException
	 | Mult(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tint else raise TypeException
	 | Sub(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tint else raise TypeException
	 | Div(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tint else raise TypeException
	 | Rem(t1,t2) ->  if (get_type g t1)=Tint && (get_type g t2)=Tint then Tint else raise TypeException
	 | Abs(t1) -> if (get_type g t1)=Tint then Tint else raise TypeException
	 | Negative(t1) -> if (get_type g t1)=Tint then Tint else raise TypeException
	 | Not(t1) -> if (get_type g t1)=Tbool then Tbool else raise TypeException
	 | Equals(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tbool else raise TypeException
	 | GreaterTE(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tbool else raise TypeException
	 | LessTE(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tbool else raise TypeException
	 | GreaterT(t1,t2) ->if (get_type g t1)=Tint && (get_type g t2)=Tint then Tbool else raise TypeException
	 | LessT(t1,t2) -> if (get_type g t1)=Tint && (get_type g t2)=Tint then Tbool else raise TypeException
	 | IfThenElse(t1,t2,t3) -> if (get_type g t1)=Tbool && (get_type g t2)=(get_type g t3) then (get_type g t2) else raise TypeException
	 | Conjunction(t1,t2) -> if (get_type g t1)=Tbool && (get_type g t2)=Tbool then Tbool else raise TypeException
	 | Disjunction(t1,t2) -> if (get_type g t1)=Tbool && (get_type g t2)=Tbool then Tbool else raise TypeException
	 | Tuple(i,l) -> (if(List.length(l)=i) then Ttuple(List.map (get_type g) l)
	  					else raise IllformedStack )
	 | Project((i,n),t1) -> (match (get_type g t1) with 
	 						Ttuple(l)-> (if (i<=n)&&(i>=1) then (List.nth l (i-1)) else raise IllformedStack)
	 						|_ -> raise TypeException)
	 | Let(d,t1) ->	get_type ((get_yield g d)@g) t1   
	 | FunctionAbstraction(i,t1) -> Tfunc((get_vartype g i),(get_type g t1))
	 | FunctionAbstraction_mod(i,tt,t1) ->Tfunc(tt,(get_type ((i,tt)::g) t1))
	 | FunctionCall(t1,t2) -> (match (get_type g t1) with Tfunc(tt1,tt2) -> (if (get_type g t2)=tt1 then tt2 else raise TypeException)
	 	 											|_ -> raise TypeException)


and 
	hastype g e t = try ( match e with
	N(i)->if t = Tint then true else false
	 | B(i)-> if t= Tbool then true else false
	 | Var(i)-> if (get_vartype g i) = t then true else false
	 | InParen(t1) -> (hastype g t1 t)
	 | Add(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tint then true else false
	 | Mult(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tint then true else false
	 | Sub(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tint then true else false
	 | Div(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tint then true else false
	 | Rem(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tint then true else false
	 | Abs(t1) -> if (hastype g t1 Tint)=true && (t=Tint) then true else false
	 | Negative(t1) -> if (hastype g t1 Tint)=true && (t=Tint) then true else false
	 | Not(t1) -> if (hastype g t1 Tbool)=true && t=Tbool then true else false
	 | Equals(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tbool then true else false
	 | GreaterTE(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tbool then true else false
	 | LessTE(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tbool then true else false
	 | GreaterT(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tbool then true else false
	 | LessT(t1,t2) -> if (hastype g t1 Tint)=true && (hastype g t2 Tint)=true && t=Tbool then true else false
	 | IfThenElse(t1,t2,t3) -> if (hastype g t1 Tbool)=true  && (get_type g t2)=(get_type g t3)then true else false
	 | Conjunction(t1,t2) -> if (hastype g t1 Tbool)=true && (hastype g t2 Tbool)=true && t=Tbool then true else false
	 | Disjunction(t1,t2) -> if (hastype g t1 Tbool)=true && (hastype g t2 Tbool)=true && t=Tbool then true else false
	 | Tuple(i,l) -> if (get_type g e) = t then true else false
	 | Project((i,n),t1) -> if (get_type g e) = t then true else false
	 | Let(d,t1) ->	if (get_type g e) = t then true else false
 	 | FunctionAbstraction(i,t1) -> (match t with
 	  	 							Tfunc(t2,t3) -> (hastype ((i,t2)::g) t1 t3)
 	  	 							|_-> false)
 	 | FunctionAbstraction_mod(i,tt,t1) ->(match t with
 	  	 							Tfunc(t2,t3) -> if tt=t2 then (hastype ((i,tt)::g) t1 t3) else false
 	  	 							|_-> false)
	 | FunctionCall(t1,t2) -> (match t1 with 
	 	 							FunctionAbstraction(i,t3)-> (hastype ((i,(get_type g t2))::g) t3 t)
	 	 							| FunctionAbstraction_mod(i,tt,t3) ->(if tt=(get_type g t2) then (hastype ((i,tt)::g) t3 t) else false)
	 	 							|_-> hastype g t1 (Tfunc((get_type g t2),t)))	 
	 ) with Varnotfound->false| TypeException->false

and
	get_yield g d =try ( match d with
	Simple(i,t1) -> [(i,get_type g t1)]
	| Simple_mod(i,t,t1) -> if((hastype ((i,t)::g) t1 t)) then [(i,t)] else raise TypeException 
	| Sequence(dl) ->  (sequence_help g dl [])
	| Parallel(dl) -> (* (parallel_help g dl []) *)  List.flatten(List.map (get_yield g) dl) 
	| Local(d1,d2) -> (get_yield ((get_yield g d1)@g) d2)

) with Varnotfound-> raise Varnotfound| TypeException-> raise TypeException
and
	sequence_help g dl l= (match dl with 
	 		[] -> l
	 		| x::xs -> (sequence_help ((get_yield g x)@g) xs ((get_yield g x)@l)))
(* and
	parallel_help g dl l=(match dl with
	 		[] -> l
	 		|x::xs-> if (common_var (get_yield g x) l)=true then raise DefException
	 			 		else ( parallel_help ((get_yield g x)@g) xs ((get_yield g x)@l) ) )     			 *)		
and	
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
	yields g d g_dash = try(if (get_yield g d)=g_dash then true else false) with Varnotfound->false| TypeException->false
;;

