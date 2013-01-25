(* HOMEWORK 1 *)

(* For convenience, date is a 3-tuple int*int*int *)
type date = int * int * int

(* Also for convenience, this way more legible code is achieved *)
fun year  (d : date) = #1 d
fun month (d : date) = #2 d
fun day   (d : date) = #3 d

(* Question 1 *)
(* date * date -> bool *)
fun is_older (first : date, second : date) =
  let
    fun product (date) =
      year(date) * 365 + month(date) * 31 + day(date)
  in
    product first < product second
  end

(* Question 2 *)
(* date list * int -> int *)
fun number_in_month (dates : date list, m : int) =
  if null dates then
    0
  else if month(hd dates) = m then
    1 + number_in_month(tl dates, m)
  else
    number_in_month(tl dates, m)

(* Question 3 *)
(* date list * int list -> int *)
fun number_in_months (dates : date list, months : int list) =
  if null dates orelse null months then
    0
  else
    number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Question 4 *)
(* date list * int -> date list  *)
fun dates_in_month (dates: date list, m: int) =
  if null dates then
    [ ]
  else if month(hd dates) = m then
    hd dates :: dates_in_month(tl dates, m)
  else
    dates_in_month(tl dates, m)

(* Question 5 *)
(* date list * int list -> date list  *)
fun dates_in_months (dates : date list, months : int list) =
  if null dates orelse null months then
    [ ]
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Question 6 *)
(* string list * int -> string *)
fun get_nth (xs : string list, n : int) =
  if n = 1 then
    hd xs
  else
    get_nth(tl xs, n - 1)

(* Question 7 *)
(* date -> string  *)
fun date_to_string (d : date) =
  let
    val ms = ["January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]
  in
    get_nth(ms, month d) ^ " " ^ Int.toString(day d) ^ ", " ^ Int.toString(year d)
  end

(* Question 8 *)
(* int * int list -> int  *)
fun number_before_reaching_sum (sum : int, xs : int list) =
  let
    fun iter (count : int, sum: int, xs : int list) =
      if sum - hd xs <= 0 then
        count
      else
        iter(count + 1, sum - hd xs, tl xs)
  in
    iter(0, sum, xs)
  end

(* Question 9 *)
(* int -> int  *)
fun what_month (days : int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(days, month_days) + 1
  end

(* Question 10 *)
(* int * int -> int list *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2 then
    [ ]
  else
    what_month(day1) :: month_range(day1 + 1, day2)

(* Question 11 *)
(* date list -> date option *)
fun oldest (dates : date list) =
  if null dates then
    NONE
  else
    let
      val old = oldest(tl dates)
    in
      if isSome old andalso is_older(valOf old, hd dates) then
        old
      else
        SOME(hd dates)
    end

(* Question 12: Challenge problem *)
(* checks if x is contained in the xs list: int * int list -> bool *)
fun contains (x : int, xs : int list) =
  if null xs then
    false
  else if hd xs = x then
    true
  else
    contains(x, tl xs)

(* removes duplicates from a given list: int list -> int list *)
fun remove_dups (xs : int list) =
  let
    fun iter (xs : int list,  ys : int list) =
      if null xs then
        ys
      else if contains(hd xs, ys) then
        iter(tl xs, ys)
      else
        iter(tl xs, ys @ [hd xs])
  in
    iter(xs, [ ])
  end

(* date list * int list -> int *)
fun number_in_months_challenge (dates : date list, months : int list) =
  if null dates orelse null months then
    0
  else
    number_in_months(dates, remove_dups(months))

(* date list * int list -> int *)
fun dates_in_months_challenge (dates : date list, months : int list) =
  if null dates orelse null months then
    [ ]
  else
    dates_in_months(dates, remove_dups(months))

(* Question 13: Challenge problem *)
(* date -> bool *)
fun reasonable_date (dt : date) =
  let
    fun leap_year (y : int) =
      y mod 400 = 0 orelse (y mod 100 <> 0 andalso y mod 4 = 0)

    fun index (xs : int list, n : int) =
      if n = 1 then hd xs else index(tl xs, n - 1)

    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val y = year(dt)
    val m = month(dt)
    val d = day(dt)
  in
    if y < 1 orelse m < 1 orelse m > 12 orelse d < 1 orelse d > 31 then
      false
    else if leap_year y andalso m = 2 then
      d <= 29
    else
      d <= index(month_days, m)
  end

