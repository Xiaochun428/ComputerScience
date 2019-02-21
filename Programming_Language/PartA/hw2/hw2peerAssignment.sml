(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (_, []) = NONE
	| all_except_option (s, hd::tl) =
		if s = hd
		then SOME tl
		else
			case all_except_option(s, tl) of
				SOME tl => SOME (hd::tl)
			| _ => NONE

fun get_substitutions1 ([], _) = []
	| get_substitutions1 (hd::tl, s) =
		case all_except_option(s, hd) of
			NONE => get_substitutions1(tl, s)
		| SOME subs => subs @ get_substitutions1(tl, s)

fun get_substitutions2 (substitutions, s) =
	let
		fun aux ([], _, acc) = acc
			| aux (hd::tl, s, acc) =
			case all_except_option(s, hd) of
				NONE => aux(tl, s, acc)
			| SOME subs => aux(tl, s, acc @ subs)
	in
		aux(substitutions, s, [])
	end

fun similar_names(substitutions, {first=a, middle=b, last=c}) =
	let
		fun aux([]) = []
			|	aux(hd::tl) = {first=hd, middle=b, last=c}::aux(tl)
	in
		{first=a,middle=b,last=c}::aux(get_substitutions2(substitutions, a))
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (Spades,_) = Black
	|	card_color (Clubs, _) = Black
	| card_color (_, _) = Red

fun card_value (_, Num i) = i
	|	card_value (_, Ace) = 11
	|	card_value (_, _) = 10

fun remove_card (cs, c, e) =
	case all_except_option(c, cs) of
		NONE => raise e
	| SOME cs' => cs'

fun all_same_color ([]) = true
	|	all_same_color (hd::[]) = true
	| all_same_color (hd::neck::tl) = card_color(hd) = card_color(neck) andalso all_same_color(neck::tl)

fun sum_cards cards =
	let
		fun aux ([], acc) = acc
			|	aux (hd::tl, acc) = aux(tl, acc + (card_value hd))
	in
		aux(cards, 0)
	end

fun score (cards, goal) =
	let
		val sum = sum_cards cards
		val preliminary_score =
			if sum > goal
			then 3 * (sum - goal)
			else goal - sum
	in
		if all_same_color cards
		then preliminary_score div 2
		else preliminary_score
	end

fun officiate (cards, moves, goal) =
	let
		fun aux (cards, moves, held_cards) =
			case moves of
				[] => score(held_cards, goal)
			|	(Discard card)::moves_left => aux(cards, moves_left, remove_card(held_cards, card, IllegalMove))
			|	Draw::moves_left =>
				case cards of
					[] => aux(cards, [], held_cards)
				|	hd::tl =>
					if sum_cards(hd::held_cards) > goal
					then aux(tl, [], hd::held_cards)
					else aux(tl, moves_left, hd::held_cards)
	in
		aux(cards, moves, [])
	end



( * Dan Grossman, Coursera PL, HW2 Provided Code *) 

  (* if you use this function to compare two strings (returns true if the same
   *    string), then you avoid several of the functions in problem 1 having
   *       polymorphic types that may be confusing *)
  fun same_string(s1 : string, s2 : string) =
  s1 = s2

  (* put your solutions for problem 1 here *)
  fun all_except_option (s, ls) =
      case ls of
         [] => NONE
        | x::xs' => if same_string(x,s)
                    then SOME xs'
                    else
                      let val res = all_except_option(s, xs')
                      in
                        case res of
                          NONE => NONE
                        | SOME res' => SOME (x::res')
                      end

  fun get_substitutions1 (lstlst, s) =
      case lstlst of
         [] => []
        |x::xs' => let val re = all_except_option(s,x)
                   in
                     case re of
                        NONE => get_substitutions1(xs', s)
                       |SOME res => res @ get_substitutions1(xs', s)
                    end

  fun get_substitutions2 (lstlst,s) =
      let fun f (lstlst, acc) =
              case lstlst of
                  [] => acc
                | x::xs' => let val re = all_except_option(s,x)
                            in case re of
                                  NONE => f(xs', acc)
                                | SOME res => f(xs', res @ acc)
                            end
      in f(lstlst, [])
      end

  fun similar_names (lstlst, fullname) =
      let val {first=x, middle=y, last=z} = fullname
      in
        let val re = get_substitutions1(lstlst, x)
            fun f (lst, acc) =
              case lst of
                [] => acc
                |l::ls' => f(ls', acc @ [{first=l, middle=y,last=z}])
        in f(re, [fullname])
        end
      end

  (* you may assume that Num is always used with values 2, 3, ..., 10
  *    though it will not really come up *)
  datatype suit = Clubs | Diamonds | Hearts | Spades
  datatype rank = Jack | Queen | King | Ace | Num of int
  type card = suit * rank

  datatype color = Red | Black
  datatype move = Discard of card | Draw

  exception IllegalMove

  (* put your solutions for problem 2 here *)

  fun card_color (card)=
      case (card) of
        (Spades, _) => Black
       |(Clubs, _) => Black
       |(Diamonds, _) => Red
       |(Hearts, _)=> Red

  fun card_value (card) =
      case card of
        (_, Num(value) ) => value
      | (_,  Ace) => 11
      | (_,  _) => 10

  fun remove_card (cs, c, e) =
      case cs of
           [] => raise e
          |x::xs' => if x = c
                     then xs'
                     else x::remove_card(xs', c, e)

  fun all_same_color (cs) =
      case cs of
          _::[] => true
        | [] => true
        | x::x'::rest => (card_color(x) = card_color(x') andalso
        all_same_color(x'::rest))

  fun sum_cards (cs) =
      let fun f(cs, acc) =
            case cs of
               [] => acc
              |x::xs => f(xs, card_value(x)+acc)
      in
        f(cs, 0)
      end

  fun score (held, goal) =
      let val res = sum_cards(held) - goal
      in
        let val re =
            if res > 0 then 3 * res
            else ~res
        in
            if all_same_color(held)
            then re div 2
            else re
        end
      end

  fun officiate (cs, mv, goal) =
    (* define a local recursive helper function several arguements repr. cur.
     * state of the game
     * starts with eld card being the empty list
     * ends if mv is empty
     * if discard card c cont', if c not in held IllegalMove *)
      let fun round1 (held, cs, mv, goal) =
          case (held, cs, mv, goal) of
            (held, _, [], goal) => score(held, goal)
           |(held, [], _, goal) => score(held, goal)
           |(held, c::cs', Draw::mv', goal) => if sum_cards(c::held) > goal
                                               then score(c::held, goal)
                                               else round1(c::held, cs', mv', goal)
           |(held, cs, Discard(card)::mv', goal) => let fun disc (card, held) =
                                                  case held of
                                                    [] => raise IllegalMove
                                                   |h::hs => if h = card then hs
                                                             else h::disc(card,hs)
                                                  in
                                                    round1(disc(card, held), cs,
                                                    mv', goal)
                                                  end
      in
        round1([],cs, mv, goal)
      end
