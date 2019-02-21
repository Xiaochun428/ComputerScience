val check_pat =
  let
    fun variable_strings p =
      case p of
	    Wildcard          => []
	  | Variable x        => [x]
	  | TupleP ps         => foldl (fn (p,i) =>  i @ variable_strings p) [] ps
	  | ConstructorP(_,p) => variable_strings p
	  | _                 => []
    fun has_repeats strings =
      case strings of
           [] => false
         | x :: [] => false
         | x :: xs => List.exists (fn y => y = x) xs orelse has_repeats xs
  in
    not o has_repeats o variable_strings
  end

fun match (v, p) =
  case (p, v) of
       (Wildcard, _) => SOME []
     | (Variable s, value) => SOME [(s, value)]
     | (UnitP, Unit) => SOME []
     | (ConstP x, Const y) => if x = y then (SOME []) else NONE
     | (TupleP ps, Tuple vs) =>
         let
           val pairs = ListPair.zip (vs, ps)
         in
           then all_answers match pairs
           else NONE
         end
     | (ConstructorP (s1, pat), Constructor (s2, value)) =>
         if s1 = s2 then match (value, pat) else NONE
     | _ => NONE

fun first_match v patterns =
  SOME (first_answer (fn p => match (v, p)) patterns)
  handle NoAnswer => NONE




  exception NoAnswer

  datatype pattern = Wildcard
  		 | Variable of string
  		 | UnitP
  		 | ConstP of int
  		 | TupleP of pattern list
  		 | ConstructorP of string * pattern

  datatype valu = Const of int
  	      | Unit
  	      | Tuple of valu list
  	      | Constructor of string * valu

  fun g f1 f2 p =
      let
  	val r = g f1 f2
      in
  	case p of
  	    Wildcard          => f1 ()
  	  | Variable x        => f2 x
  	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
  	  | ConstructorP(_,p) => r p
  	  | _                 => 0
      end

  (**** for the challenge problem only ****)

  datatype typ = Anything
  	     | UnitT
  	     | IntT
  	     | TupleT of typ list
  	     | Datatype of string

  fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

  fun longest_string1 xs = List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

  fun longest_string2 xs = List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

  fun longest_string_helper f = List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) ""

  val longest_string3 = longest_string_helper (fn (a,b) => a>b)

  val longest_string4 = longest_string_helper (fn (a,b) => a>=b)

  val longest_capitalized = longest_string1 o only_capitals

  val rev_string = String.implode o List.rev o String.explode

  fun first_answer f xs =
      case xs of
  	x::xs' => (case f x of
  		      SOME e => e
  		    | NONE  => first_answer f xs')
        | _ => raise NoAnswer

  fun all_answers f xs =
      let fun whatever f xs acc =
  	    case xs of
  		x::xs' => (case f x of
  			       SOME e => whatever f xs' (e @ acc)
  			     | NONE  => NONE)
  	      | [] => SOME acc
      in whatever f xs []
      end

  fun count_wildcards p = g (fn () => 1) (fn _ => 0) p

  fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p

  fun count_some_var (s,p) = g (fn () => 0) (fn x => if s=x then 1 else 0) p

  fun check_pat p =
      let
  	fun list_check xs =
  	    case xs of
  		[] => false
  	      | x::[] => true
  	      | x::xs' => if List.exists (fn y => x=y) xs'
  			  then false else list_check xs'
  	fun list_of_strings p =
  	    case p of
  		Variable x        => [x]
  	      | TupleP ps         => List.foldl (fn (x,y) => list_of_strings x @ y) [] ps
  	      | ConstructorP(_,p) => list_of_strings p
  	      | _                 => []
      in (list_check o list_of_strings) p
      end

  fun match (v,p) =
      case (v,p) of
  	(_,Wildcard) => SOME[]
        | (_,Variable s) => SOME[(s,v)]
        | (Unit,UnitP) => SOME[]
        | (Const x, ConstP y) => if x = y then SOME [] else NONE
        | (Tuple vs,TupleP ps) =>	if (List.length vs = List.length ps)
  				then all_answers match (ListPair.zip(vs,ps))
  				else NONE
        | (Constructor(s2,v'),ConstructorP(s1,p')) => if s1 = s2 then match(v',p') else NONE
        | _ => NONE

  fun first_match v ps =
      case ps of
  	[] => NONE
        | _ => SOME (first_answer (fn x => match(v,x)) ps) handle NoAnswer => NONE

  										
