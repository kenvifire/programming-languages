fun is_older ((y1, m1, d1), (y2, m2, d2)) =
    let
      val days1 = y1 * 365 + m1 * 30 + d1
      val days2 = y2 * 365 + m2 * 30 + d2
    in
      days1 < days2
    end 

fun number_in_month(d_list : (int*int*int) list,month) = 
   if null d_list 
   then 0
   else
   if #2 (hd d_list) = month 
   then 1 + number_in_month(tl d_list, month)
   else
   number_in_month(tl d_list, month) 

fun number_in_months(d_list : (int*int*int) list, m_list : int list) = 
  if null m_list 
  then 0
  else
  number_in_month(d_list, hd m_list) + number_in_months(d_list, tl m_list)

fun dates_in_month(d_list : (int*int*int) list, month) = 
  if null d_list
  then []
  else
  if month = #2 (hd d_list) 
  then 
  (hd d_list)::dates_in_month(tl d_list,month)
  else
  dates_in_month(tl d_list, month)

fun dates_in_months(d_list : (int*int*int) list, m_list : int list) =
  if null m_list
  then []
  else
  dates_in_month(d_list, hd m_list)@dates_in_months(d_list, tl m_list)

fun get_nth(str_list : string list, n : int) = 
  if n = 1 
  then 
      hd str_list
  else 
      get_nth(tl str_list, n-1)

fun date_to_string(date : int*int*int) =
  let 
  val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
  in
  get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(# 1 date)
  end

fun number_before_reaching_sum(sum : int, i_list : int list) = 
    if sum <= (hd i_list)
    then 
       0        
    else 
        1 + number_before_reaching_sum(sum - (hd i_list), tl i_list)

fun what_month(day : int) = 
  let 
      val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, months) + 1
  end

fun month_range(day1 : int, day2 :int) = 
    if day1 > day2
    then
        []
    else 
       what_month(day1)::month_range(day1+1, day2)

fun oldest(d_list : (int*int*int) list) = 
    if null d_list
    then NONE
    else
        let val tl_ans = oldest(tl d_list)
        in if isSome tl_ans andalso is_older(valOf tl_ans, hd d_list)
           then tl_ans
           else SOME (hd d_list)
        end

