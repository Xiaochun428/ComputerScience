(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard (* p *)
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern
						
datatype valu = Const of int  (* v *) 
 	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let val r = g f1 f2
    in 	case p of
	    Wildcard => f1 ()
	  | Variable x => f2 x
	  | TupleP ps => List.foldl (fn (p1,i) => (r p1) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _ => 0 end      

	
datatype typ = Anything
	     | UnitT 	 | IntT
	     | TupleT of typ list
	     | Datatype of string   (**** you can put all your code here ****)

			       
(*question 1  val g = fn : (unit -> int) -> (string -> int) -> pattern -> int
   val only_capitals = fn : string list -> string list *)

			       

val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str,0)))

				
(* question 2 val longest_string1 = fn : string list -> string *)
val longest_string1  = foldl (fn(x,acc)=> if String.size(x) > String.size(acc)
					  then x
					  else acc) "" 

			     
(* quesiton 3 same as q2 but reurns the string closest to the end of the list *)
val longest_string2  = foldl (fn(x,acc)=> if String.size(x) >= String.size(acc)
					  then x
					  else acc) "" 
						     
(* quesiton 4 longest_string_helper has type (int * int -> bool) -> string list -> string *)
val longest_string_helper =
    let
	fun helper1 acc f xs =
	    case xs of 
		[] => acc
	      | x::xs' => helper1 (if f(String.size(x),String.size(acc)) then x else acc) f xs'
    in helper1 ""
    end
	

val longest_string3 = longest_string_helper (fn (x1,x2)=> x1 > x2)

					    
val longest_string4 = longest_string_helper (fn (x1,x2)=> x1 >= x2)

					    
(* question 5 val longest_capitalized = fn : string list -> string *)
val longest_capitalized = longest_string1 o only_capitals

						
(* quesiton 6 rev_string = fn : string -> string *)
val rev_string = String.implode o rev o String.explode

					    
(* question 7 val first_answer = fn : ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME y => y 

(* question 8 val all_answers = fn : ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f xs =
    let fun helper1 acc f xs =
	    case (acc,xs) of
		(acc,[]) => SOME acc
	       | (acc,x::xs') => case f x of
				  NONE => NONE
				 | SOME y =>  helper1 (acc@(valOf(f x))) f xs'
    in helper1 [] f xs
    end
val count_wildcards1 = g (fn () => 1) (fn _ => 0)
	

(* question 9 (a) val count_wildcards = fn : pattern -> int *)	
fun count_wildcards p =
    let val r = count_wildcards
    in  case p of
	Wildcard => 1
      | TupleP ps=> List.foldl (fn(p1,i) => (r p1) + i) 0 ps
      | ConstructorP(_,p1) => (count_wildcards p1) 
      | _  => 0 end

val count_wildcards1 = g (fn () => 1) (fn _ => 0)
			 
(* question 9 (b)  val count_wild_and_variable_lengths = fn : pattern -> int *)
fun count_wild_and_variable_lengths p =
    let val r = count_wild_and_variable_lengths
    in 	case p of
	    Wildcard => 1
	  | Variable x => String.size x
	  | TupleP ps => List.foldl (fn (p1,i) => (r p1) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _ => 0 end
	
val count_wild_and_variable_lengths = g (fn () => 1) String.size
	
(* quesiton 9 (c) val count_some_var = fn : string * pattern -> int *)
fun count_some_var (s,p) =
    case (s,p) of
	(s,Variable s1) => if s = s1
			   then 1
			   else 0
      | (s,TupleP ps) => List.foldl (fn(p1,i) => count_some_var (s,p1) + i) 0 ps
      | (s,ConstructorP(s1,p1)) => if s = s1
				   then 1 +  count_some_var(s,p1)
				   else count_some_var(s,p1)
      | (_,_) => 0

fun count_some_var1 (x,p) = g (fn () => 0) (fn s => if s = x then 1 else 0) p
		
(* quesiton 10 val check_pat = fn : pattern -> bool *)

fun strings_in_pattern p =
    let val r = strings_in_pattern
    in case p of 	   
	Variable s1 => [s1]
      | TupleP ps => List.foldl (fn(p,i) => i @ (r p)) [] ps
      | ConstructorP(s1,p1) => [s1] @ (r p1)
      | _ => [] end
fun same_string (x1:string, x2:string) = x1 = x2				       
fun diff_string s xs =
    case xs of 
	[] => true
      | x::xs' => if same_string (x, s) then false else diff_string s xs'
fun unique xs =
    case xs of
	[] => true
     | x::xs' => (diff_string x xs') andalso unique xs'
val check_pat = unique o strings_in_pattern


(* quesiton 11 val match = fn : valu * pattern -> (string * valu) list option *)
fun match (v, p) =	
    case (v, p) of
	(v,Variable s1) => SOME [(s1,v)]
      | (v,Wildcard) => SOME []
      | (Unit,UnitP) => SOME []
      | (Const x1,ConstP x2) => if x1 = x2
				then SOME []
				else NONE 
      | (Tuple vs,TupleP ps) => if List.length(vs) = List.length(ps)
				then all_answers (fn x =>match x) (ListPair.zip(vs,ps))
				else NONE
      | (Constructor(s1,v1),ConstructorP(s2,p1)) => if s1 = s2
						    then match(v1,p1)
						    else NONE
      | _ => NONE
		     
(* question 12 val first_match = fn : valu -> pattern list -> (string * valu) list option *)


fun first_match v ps = SOME(first_answer (fn p => match(v,p)) ps) handle NoAnswer =>NONE
   

