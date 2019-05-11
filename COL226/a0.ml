type sign = Neg | NonNeg;;

type bigint= sign * int list;;

exception Divisionbyzero;;

let minus (a:bigint) =
	if (fst a) = Neg then
		(NonNeg, snd a)
	else
		(Neg, snd a)
	;;		

(* function to calculate absolute value *)
let abs (a:bigint) = 
	(NonNeg, snd a)	
	;;	 

(*function to remove unnecessary(leading) zeros*)
let rec rem_zero a= match a with
	[] ->[]
	|x::xs -> if x=0 then rem_zero xs
				else x::xs
	;;			

let rec equalh a b = match a with 
	[] -> (match b with 
		[] -> true
		| x::xs -> if x=0 then equalh a xs
					else false )
	| y::ys -> match b with 
		[] -> if y=0 then equalh ys b
					else false
		| x::xs -> if x = y then equalh ys xs
				else false		
	;;

let eq (a:bigint) (b:bigint) = 
	if (fst a) != (fst b) then false
	else
		equalh (snd a) (snd b)
	;;	

(* compares int list to int list of same size *)
let rec greaterh a b= match a with
	[] -> false
	| y::ys -> match b with 
		[] -> true
		| x::xs -> if y>x then true
				else if y<x then false
				else greaterh ys xs
	;;

(* compares int list to int list *)
let rec greater a b=	(*non reversed inputs and outputs*)
	if List.length(a) < List.length(b) then 
		(match b with
			[]->false
			|x::xs -> if x=0 then greater a xs
				else false  )
		else if List.length(a) > List.length(b) then
		(match a with
			[]-> true
			| x::xs -> if x=0 then greater xs b 
				else true)
		else	
		greaterh a b
	;;	

(*greater than*)
let gt (a:bigint) (b:bigint) =
	if (fst a) = Neg && (fst b) = NonNeg then
		false
	else if (fst a) = NonNeg && (fst b) = Neg then	
		true 
	else if (fst a) = NonNeg && (fst b) = NonNeg then		
		greater (snd a) (snd b)
	else
		greater (snd b) (snd a)
	;;

(* compares int list to int list *)
let	lesser a b =	(*non reversed inputs and outputs*)
	if (greater a b) = false && (equalh a b)==false then
		true
	else
		false
	;;

(*less than*)	
let lt (a:bigint) (b:bigint) =	
	if (gt a b) =false && (eq a b) =false then
		true
	else
		false
	;;	

(*greater than or equal to*)
let geq	(a:bigint) (b:bigint)=
	if lt a b =true then
		false
	else 
		true
	;;		

(*less than or equal to*)
let leq	(a:bigint) (b:bigint)=
	if gt a b =true then
		false
	else
		true
	;;		

(*helpers for add function*)
let d_sum a b c = ((a+b+c) mod 10);;

let d_carry a b c = ((a+b+c)/10);;

let rec addh a b c = match a with 	(*reversed inputs and outputs*)
	[]-> ( match b with
		[]-> if c==1 then [1]
			else []
		| x::xs -> if c==0 then x::xs
				else (d_sum 0 x 1) :: addh [] xs (d_carry 0 x 1) )
	| y::ys ->  match b with
		[] -> if c==0 then y::ys
			else (d_sum y 0 1) :: addh ys [] (d_carry y 0 1)
		| x::xs -> (d_sum y x c) :: addh ys xs (d_carry y x c)
	;;	

(* helpers for subtract function*)
let d_sub a b c = ((a-c-b+10) mod 10)	;;

let d_borrow a b c = ((b+c-a+9)/10)	;;

let rec subh a b c =match a with 	(*reversed inputs and outputs*)
	[] ->( match b with
		[] -> if  c==1 then[0]
			else []	
		| x::xs -> if c==0 then x::xs
				else (d_sub 0 x 1)	:: subh [] xs (d_borrow 0 x 1))
	| y::ys -> match b with
		[] -> if c=0 then y::ys
			else (d_sub y 0 1) :: subh ys [] (d_borrow y 0 1)
		| x::xs -> (d_sub y x c) :: subh ys xs (d_borrow y x c)
	;;		

(*addition function*)
let add (a:bigint) (b:bigint) = 
	let (a1,a2)=(List.rev (snd a),List.rev (snd b)) in
	if (((fst a) = NonNeg) && ((fst b) = NonNeg )) then 
		(NonNeg , rem_zero(List.rev(addh a1 a2 0)))
	else if	(fst a) = Neg && (fst b) = Neg then 
		(Neg,rem_zero(List.rev(addh a1 a2 0)))
	else if	(fst a) = NonNeg && (fst b) = Neg then 
		(if (greater (List.rev(a2)) (List.rev(a1)) = false) then
			(NonNeg,rem_zero(List.rev(subh a1 a2 0)))
		else 
			(Neg,rem_zero(List.rev(subh a2 a1 0)) ) )
	else
		(if (greater (List.rev(a1)) (List.rev(a2)) = false) then
			(NonNeg , rem_zero(List.rev(subh a2 a1 0)))
		else 
			(Neg, rem_zero(List.rev(subh a1 a2 0)))	)
	;;
		
(*subtraction function*)
let sub (a:bigint) (b:bigint)=
	let (a1,a2)=(List.rev (snd a),List.rev (snd b)) in
	if ((fst a) = NonNeg) && ((fst b) = NonNeg ) then 
		if greater (List.rev(a2)) (List.rev(a1)) = false then
			(NonNeg , rem_zero(List.rev(subh a1 a2 0)))
		else 
			(Neg, rem_zero(List.rev(subh a2 a1 0)))
	else if	(fst a) = Neg && (fst b) = Neg then 
		if greater (List.rev(a1)) (List.rev(a2)) = false then
			(NonNeg , rem_zero(List.rev(subh a2 a1 0)))
		else 
			(Neg, rem_zero(List.rev(subh a1 a2 0)))
	else if	(fst a) = NonNeg && (fst b) = Neg then 
		(NonNeg, rem_zero(List.rev(addh a1 a2 0)))
	else 
		(Neg, rem_zero(List.rev(addh a1 a2 0)))
	;;

(* helpers for multiply functions*)
let d_mult a b c= ((a*b+c) mod 10);;

let rec ld_mult a b c= match a with	
	[] -> (if c=0 then []
			else [c])
	| y::ys -> (d_mult y b c) :: ld_mult ys b (d_carry (b*y) 0 c)
	;;		

let rec multh a b c = match a with 	(*reversed inputs and outputs*)
	[] -> ( if c=0 then []
			else [c] )
	| y::ys -> if b=[] then
				( if c=0 then []
					else [c] )
			else addh (ld_mult b y c) (0::(multh ys b 0)) 0		
	;;	

(*multiplication function*)
let mult (a:bigint) (b:bigint)=
	let (a1,a2)=(List.rev (snd a),List.rev (snd b)) in 
	if ((fst a) = NonNeg) && ((fst b) = NonNeg ) then 
		(NonNeg , rem_zero(List.rev(multh a1 a2 0)))
	else if	(fst a) = Neg && (fst b) = Neg then 
		(NonNeg, rem_zero(List.rev(multh a1 a2 0)))
	else if	(fst a) = NonNeg && (fst b) = Neg then 
		(Neg, rem_zero(List.rev(multh a1 a2 0)))
	else 
		(Neg, rem_zero(List.rev(multh a1 a2 0)))
	;;		  			

(*helpers for divide function*)
let divhh a b= 
	if (greater (List.rev(ld_mult b 2 0)) (List.rev(a))) =true then
		1
	else if (greater (List.rev(ld_mult b 3 0)) (List.rev(a))) =true then
		2
	else if (greater (List.rev(ld_mult b 4 0)) (List.rev(a))) =true then
		3
	else if (greater (List.rev(ld_mult b 5 0)) (List.rev(a))) =true then
		4
	else if (greater (List.rev(ld_mult b 6 0)) (List.rev(a))) =true then
		5
	else if (greater (List.rev(ld_mult b 7 0)) (List.rev(a))) =true then
		6
	else if (greater (List.rev(ld_mult b 8 0)) (List.rev(a))) =true then
		7
	else if (greater (List.rev(ld_mult b 9 0)) (List.rev(a))) =true then
		8
	else if (greater (List.rev(ld_mult b 10 0)) (List.rev(a)))=true then	
		9
	else
		0
	;;

let rec divh a b c = match a with (*non reversed inputs and outputs*)
	[] -> (match b with 
		[]-> raise Divisionbyzero
		|[0]-> raise Divisionbyzero
		| x::xs->[]) 
	| y::ys -> 
		if b=[] || b=[0] then raise Divisionbyzero
		else
			if (greater (b) (List.rev(y::c))) then			
				0::divh ys b (y::c)
			else 
			let q= divhh (y::c) (List.rev(b)) in
			if (equalh (subh (y::c) (ld_mult (List.rev(b)) q 0) 0) []) = true then
				q:: divh ys b []	
			else
				q:: divh ys b (subh (y::c) (ld_mult (List.rev(b)) q 0) 0)	
	;;

(*division function*)
let div (a:bigint) (b:bigint)=
	let (a1,a2)=(snd a,snd b) in 
	if ((fst a) = NonNeg) && ((fst b) = NonNeg ) then 
		(NonNeg , rem_zero(divh a1 a2 []))
	else if	(fst a) = Neg && (fst b) = Neg then 
		(NonNeg, rem_zero(divh a1 a2 []))
	else if	(fst a) = NonNeg && (fst b) = Neg then 
		(Neg, rem_zero(divh a1 a2 []))
	else 
		(Neg, rem_zero(divh a1 a2 []))
	;;	

(*remainder function*)
let rem (a:bigint) (b:bigint)=
	let (a1,a2)=(snd a,snd b) in 
	if ((fst a) = NonNeg) then 
		(NonNeg ,rem_zero(List.rev(subh (List.rev(a1)) (List.rev(rem_zero(List.rev(multh (List.rev(divh a1 a2 [])) (List.rev(a2)) 0)))) 0)))
	else 	
		(Neg ,rem_zero(List.rev(subh (List.rev(a1)) (List.rev(rem_zero(List.rev(multh (List.rev(divh a1 a2 [])) (List.rev(a2)) 0)))) 0)))
	;;


let printh a = 
	let str =String.concat "" (List.map string_of_int a) in
		if str ="" then "0" 
    	else str
	;;

(*Function to present the result in the form of a string*)
let print_num (a:bigint) =
	if  (fst a)=(Neg) then
		(if  (snd a)=[0] || (snd a)=[]  then
			printh [0]
		else	
			"-"^printh (snd a))
	else
		printh (snd a)	
	;;	


let rec mkh a l= 
	if a=0 then l
	else
		mkh (a/10) ((a mod 10)::l)
	;;

(*Conversion function from OCaml int to bigint*)
let mk_big a =
	if a<0 then (Neg,mkh (-a) [])
	else
		(NonNeg,mkh a [])
;;