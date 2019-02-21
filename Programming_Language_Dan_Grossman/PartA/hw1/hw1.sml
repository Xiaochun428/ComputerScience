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


fun number_in_month (xs : (int*int*int) list, number : int) =
    if null xs
    then 0
    else (if (#2 (hd xs)) = number then 1 else 0)  + number_in_month((tl xs),number)


fun number_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then 0
    else number_in_month(xs, (hd ys))+number_in_months(xs, (tl ys))


fun dates_in_month (xs : (int*int*int) list, number : int) =
    if null xs
    then []
    else if (#2 (hd xs)) = number
    then (hd xs) :: dates_in_month((tl xs), number)
    else dates_in_month((tl xs), number)


fun dates_in_months (xs : (int*int*int) list, ys : int list) =
    if null ys
    then []
    else dates_in_month (xs, (hd ys)) @ dates_in_months (xs, (tl ys))


fun get_nth (xs : string list, number : int) =
    if number = 1
    then hd xs
    else get_nth (tl xs, number - 1)


fun date_to_string (date : int*int*int) =
    let val name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 +  number_before_reaching_sum((sum - hd lst), tl lst)

fun what_month (days : int) =
    let
	val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum (days, month_lengths)
    end

fun month_range (day1 : int, day2 : int) =
    let	fun countup(from : int, to : int) =
	    if from=to
	    then to::[]
	    else from :: countup(from+1,to)
    in
	if (hd (countup(day1, day2))) = day2
	then what_month((hd (countup(day1, day2)))) :: []
	else what_month((hd (countup(day1, day2)))) :: month_range(day1+1, day2)
    end


fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let
        val ans = oldest(tl dates)
    in
	if isSome ans andalso is_older(valOf ans, hd dates)
	then ans
	else SOME (hd dates)
    end
