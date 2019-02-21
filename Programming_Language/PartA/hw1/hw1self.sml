  
(*val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let
	val y1 = #1 date1
	val m1 = #2 date1
	val d1 = #3 date1
	val y2 = #1 date2
	val m2 = #2 date2
	val d2 = #3 date2
    in
	y1 < y2 orelse (y1 = y2 andalso m1 < m2)
	orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end







(* migth want to sort this first *)
(* question 1 ,looks ok for now.remeber dont try something like if e then ture else false, that is basicly the same as we value e , if e is a boolen *)
(* question 2 val number_in_month = fn : (int * int * int) list * int -> int *)
		 
fun number_in_month (xs : (int*int*int) list, number : int) =
    if null xs
    then 0    
    else (if (#2 (hd xs)) = number then 1 else 0)  + number_in_month((tl xs),number)
(* syntax error, it is very important to understand how the syntax ,tpye-cheaking ,evaluatation working.And more important is that we do it both on function bindings and function call. What is static envornment and what is dynamic envornment *)

(* question 3 val number_in_months = fn : (int * int * int) list * int list -> int *)
		      
fun number_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then 0
    else number_in_month(xs, (hd ys))+number_in_months(xs, (tl ys))

(* question 4
 val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)

fun dates_in_month (xs : (int*int*int) list, number : int) =
    if null xs
    then []
    else if (#2 (hd xs)) = number
    then (hd xs) :: dates_in_month((tl xs), number)
    else dates_in_month((tl xs), number)
		       
(* question 5 val dates_in_months =
 fn : (int * int * int) list * int list -> (int * int * int) list *)

fun dates_in_months (xs : (int*int*int) list, ys : int list) =    
    if null ys
    then []
    else dates_in_month (xs, (hd ys)) @ dates_in_months (xs, (tl ys))
							 
(* question 6 val get_nth = fn : string list * int -> string *)

fun get_nth (xs : string list, number : int) =
    if number = 1
    then hd xs
    else get_nth (tl xs, number - 1)
	     
(* question 7 val date_to_string = fn : int * int * int -> string *)
		       
fun date_to_string (date : int*int*int) =
    let val name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
																						   
(* question 8 val number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst  
    then 0
    else 1 +  number_before_reaching_sum((sum - hd lst), tl lst)  

(*question  9 val what_month = fn : int -> int *)	    
fun what_month (days : int) =
    let
	val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum (days, month_lengths)
    end
(* question 10 val month_range = fn : int*int -> int list *)
fun month_range (day1 : int, day2 : int) =   (* Note the result will have length day2 - day1 or length() if day1>day2 *)
    let	fun countup(from : int, to : int) =
	    if from=to
	    then to::[]
	    else from :: countup(from+1,to)
    in
	if (hd (countup(day1, day2))) = day2
	then what_month((hd (countup(day1, day2)))) :: []
	else what_month((hd (countup(day1, day2)))) :: month_range(day1+1, day2)
    end
(* question 11 val oldest = fn : (int * int * int) list -> (int * int * int) option *)
	
fun oldest (xs : (int*int*int) list) =
   
    let fun min_year (xs : (int*int*int) list) =
	    if null xs
	    then NONE
	    else let                                           
		fun min_nonempty (xs : (int*int*int) list) =
		    if null (tl xs)                            
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

	fun dates_in_year (xs : (int*int*int) list, year : int) =
	    if null xs
	    then []
	    else if (#1 (hd xs)) = year
	    then (hd xs) :: dates_in_year ((tl xs), year)
	    else dates_in_year ((tl xs), year)

	fun min_month (xs : (int*int*int) list) =
	    if null xs
	    then NONE
	    else let
		fun min_nonempty (xs :(int*int*int) list) =
		    if null (tl xs)

		    then #2 (hd xs)
		    else let val tl_ans = min_nonempty (tl xs)
			 in
			     if #2 (hd xs) < tl_ans
			     then #2 (hd xs)
			     else tl_ans
			 end
	    in
		SOME (min_nonempty xs)
	    end

	fun dates_in_month (xs : (int*int*int) list, month : int) =
	    if null xs
	    then []
	    else if (#2 (hd xs)) = month
	    then (hd xs) :: dates_in_month ((tl xs), month)
	    else dates_in_month ((tl xs), month)


	fun min_day (xs : (int*int*int) list) =
	    if null xs
	    then NONE
	    else let
		fun min_nonempty (xs :(int*int*int) list) =
		    if null (tl xs)
		    then #3 (hd xs)
		    else let val tl_ans = min_nonempty (tl xs)
			 in
			     if #3 (hd xs) < tl_ans
			     then #3 (hd xs)
			     else tl_ans
			 end
	    in
		SOME (min_nonempty xs)
	    end

	fun dates_in_day (xs : (int*int*int) list, day : int) =
	    if null xs
	    then []
	    else if (#3 (hd xs)) = day
	    then (hd xs) :: (dates_in_day ((tl xs), day))
	    else dates_in_day ((tl xs), day)
		     
		
			       

    in
	if null xs
	then NONE
	else SOME (hd (dates_in_day(xs,valOf(min_day(dates_in_month(xs, valOf(min_month(dates_in_year (xs, valOf(min_year(xs)))))))))))
    end





