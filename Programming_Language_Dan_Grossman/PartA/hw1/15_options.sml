(* Programming Languages, Dan Grossman *)
(* Section 1: Options *)

(* badly named: evaluates to 0 on empty list *)
fun old_max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let val tl_ans = old_max(tl xs)
	in
	    if hd xs > tl_ans
	    then hd xs
	    else tl_ans
	end

(* better: returns an int option *)
fun max1 (xs : int list) =
    if null xs
    then NONE
    else 
	let val tl_ans = max1(tl xs)
	in if isSome tl_ans andalso valOf tl_ans > hd xs
	   then tl_ans
	   else SOME (hd xs)
	end

(* looks the same as max1 to clients; 
   implementation avoids valOf *)
fun max2 (xs : int list) =
    if null xs
    then NONE
    else let (* fine to assume argument nonempty because it is local *)
	fun max_nonempty (xs : int list) =
		if null (tl xs) (* xs better not be [] *)
		then hd xs
		else let val tl_ans = max_nonempty(tl xs)
		     in
			 if hd xs > tl_ans
			 then hd xs
			 else tl_ans
		     end
	in
	    SOME (max_nonempty xs)
	end

fun min_year (xs : (int*int*int) list) =
    if null xs
    then NONE
    else let (* fine to assume argument nonempty because it is local *)
	fun min_nonempty (xs : (int*int*int) list) =
		if null (tl xs) (* xs better not be [] *)
		then #1 (hd xs)
		else let val tl_ans = min_nonempty(tl xs)
		     in
			 if #1 (hd xs) < tl_ans
			 then #1 (hd xs)
			 else tl_ans
		     end
	in
	    SOME (min_nonempty xs)
	end
