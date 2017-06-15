(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str : string, str_list : string list) =
   let fun all_except_option_helper(dest : string, origin_list : string list, result : string list) = 
    case str_list of
         [] => NONE
        | head::str_tail => if same_string(dest, head) then SOME (result@str_tail)  else  all_except_option_helper(dest, str_tail,head::result)
   in
     all_except_option_helper(str, str_list,[])
   end 

fun get_substitutions1(subs_list : string list list, str : string) = 
    case subs_list of
       [] => []
       | head::subs_list_tail =>
            let val sub = all_except_option(str,head) 
            in
                if isSome sub then (valOf sub)@get_substitutions1(subs_list_tail, str) 
                else
                   get_substitutions1(subs_list_tail, str)
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
