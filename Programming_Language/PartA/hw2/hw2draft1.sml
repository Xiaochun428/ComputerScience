(* question1 (a) val all_except_option = fn : string * string list -> string list option  *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
fun all_except_option (s,L) =    
    let fun q1_helper (s,L) =
	    case(s,L)of
		(_,[]) => []
	      | (s,L1::L') => if same_string(s,L1) then L' else L1::(q1_helper(s,L'))
    in case L of
	   [] => NONE
	 | b::c => if (q1_helper(s,L))=L then NONE else SOME(q1_helper(s,L)) 
    end


(* question1 (b) val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1 (LL,s) =
    let fun q1_helper (L,s) =
	    case(L,s)of
		([],_) => []
	      | (L1::L',s) => if same_string(s,L1) then L' else L1::(q1_helper(L',s))
    in case (LL,s) of
	   ([],s) => []
	 | (LL1::LL',s) => if q1_helper(LL1,s)=LL1
			   then []@get_substitutions1(LL',s)
			   else q1_helper(LL1,s)@get_substitutions1(LL',s)
    end

	
(*question1 (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function. *)

fun get_substitutions2 (LL,s) =
    let fun aux (LL,s,acc) =
	    let fun helper2 (L,s) = 
		    let fun helper1 (L,s) =
			    case(L,s)of
				([],_) => []
			      | (L1::L',s) => if same_string(s,L1) then L' else L1::(helper1(L',s))
		    in case L of
			   [] => []
			 | L1::L' => if helper1(L,s)=L then [] else helper1(L,s)
		    end		
	    in case LL of
		   [] => acc
		 | LL1::LL' => aux(LL',s,acc@(helper2(LL1,s)))
	    end
    in aux(LL,s,[])
    end

(* question1 (d) type {first:string,middle:string,last:string} list val similar_names = 
fn : string list list * {first:string, last:string, middle:string} -> 
{first:string, last:string, middle:string} list *)

fun similar_names (LL,r) = 
    let fun helper (L,r) =
	    case (L,r) of
		([],{first=x,middle=y,last=z}) => []
	      | (L1::L',{first=x,middle=y,last=z}) => {first=L1,last=z,middle=y}::helper(L',r)
    in case (LL,r) of
	    ([], {first=x,middle=y,last=z}) => [{first=x,last=z,middle=y}]
	  | (LL1::LL',{first=x,middle=y,last=z}) => {first=x,last=z,middle=y}::helper(get_substitutions2(LL,x),r)
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
		    
fun remove_card (cs,c,e) =				  
    let fun h2 (cs,c,e,n) = 	
	    case (cs,c,e,n) of
		(c1::cs',c,e,0) => raise e
	      | (c1::cs',c,e,n) => if c1=c then cs' else c1::h2(cs',c,e,n)	      
	fun h1 (cs,c) =
	    case (cs,c) of
		([],c) => 0
	      | (c1::cs',c) => if c1=c then 1+h1(cs',c) else  h1(cs',c)			       			       
    in h2(cs,c,e,h1(cs,c))
    end
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
fun score (cs,g) =
    let fun preliminary_score (cs,g) =
	    if sum_cards(cs)>g then 3*(sum_cards(cs) - g) else g - sum_cards(cs)
    in if all_same_color(cs) then preliminary_score(cs,g) div 2 else preliminary_score(cs,g)
    end
	
(* question 2 (g) val officiate = fn : card list * move list * int -> int *)
fun officiate (cs,ms,g) =
    let fun h1(cs,ms,hc,g) =
	    case (cs,ms,hc,g) of
		([],ms,hc,g) => hc
	      | (cs,[],hc,g) => hc	    
	      | (c1::cs',Draw::ms',hc,g) => if sum_cards(c1::hc) > g
					    then c1::hc
					    else h1(cs',ms',c1::hc,g)
	      | (c1::cs',Discard(card)::ms',hc,g) => h1(cs,ms',remove_card(hc,card,IllegalMove),g)
    in score(h1(cs,ms,[],g),g)
    end

