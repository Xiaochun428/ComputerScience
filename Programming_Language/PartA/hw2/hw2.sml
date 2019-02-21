(* question1 (a) val all_except_option = fn : string * string list -> string list option  *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
	     
fun all_except_option (s,xs) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(s,x)
                  then SOME xs'
                  else case all_except_option(s,xs') of
                           NONE => NONE
			 | SOME y => SOME(x::y)


(* question1 (b) val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1 (substitutions,str) =
    case substitutions of
	[] => []
      | x::xs => case all_except_option(str,x) of
		     NONE => get_substitutions1(xs,str)
		   | SOME y => y @ get_substitutions1(xs,str)

	
(*question1 (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function. *)

fun get_substitutions1 (substitutions,str) = 
    case substitutions of 
        [] => [] 
      | x::xs => let val foo = all_except_option(str,x) 
                 in case foo of 
                        NONE => get_substitutions1(xs,str) 
                      | SOME y => y @ get_substitutions1(xs,str) 
                 end 

(* question1 (d) type {first:string,middle:string,last:string} list val similar_names = 
fn : string list list * {first:string, last:string, middle:string} -> 
{first:string, last:string, middle:string} list *)

fun similar_names (substitutions,name) =
    let 
        val {first=f, middle=m, last=l} = name
	fun make_names xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
    in
	name::make_names(get_substitutions2(substitutions,f))
    end
	
(* question2 (a) *)
	
datatype suit = Clubs
	      | Diamonds
	      | Hearts
	      | Spades
		    
datatype rank = Jack
	      | Queen
	      | King
	      | Ace
	      | Num of int
			   
type card = suit * rank

datatype color = Red
	       | Black
		     
datatype move = Discard of card
	      | Draw

exception IllegalMove
	      
fun card_color (s,r) =
    case (s,r) of
	(Hearts,r) => Red
      | (Diamonds,r) => Red
      | (_,r) => Black

(* question 2 (b) *)
fun card_value (s,r) =
    case (s,r) of
	(s,Ace) => 11
     |  (s,Num x)=> x
     | (s,_) => 10
		    
(* quesiton 2 (c)val remove_card = fn : card list * card * exn -> card list  *)
fun same_card(c1 : card, c2 : card)=
    c1=c2
	   
fun remove_card (cs,c,e) =				  
    case cs of
	[] => raise e
      | x::cs' => if same_card (x,c) then cs' else x :: remove_card(cs',c,e) 
	
(*question 2 (d)  val all_same_color = fn : card list -> bool *)
fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true
      | c1::(c2::cs') => (card_color(c1)=card_color(c2) andalso all_same_color(c2::cs')) 
			     
(*question 2 (e) val sum_cards = fn : card list -> int *)
fun sum_cards cs =
    let fun h1 (cs,sum) =	
	    case (cs,sum) of
		([],sum) => sum
	      | (c1::cs',sum) => h1(cs',card_value(c1)+sum)
    in h1(cs,0)
    end
	
(* question 2 (f) val score = fn : card list * int -> int  *)
fun score (cs,goal) =
    let
	val sum = sum_cards cs
	val preliminary_score  =
	    if sum > goal
	    then 3*(sum - goal)
	    else goal - sum
    in
	if all_same_color(cs)
	then preliminary_score div 2
	else preliminary_score
    end
	
(* question 2 (g) val officiate = fn : card list * move list * int -> int *)

fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end
	`
