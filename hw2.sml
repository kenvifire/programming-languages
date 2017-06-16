(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str : string, str_list : string list) =
   let fun all_except_option_helper(dest : string, origin_list : string list, result : string list) = 
    case origin_list of
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

fun get_substitutions2(subs_list : string list list, str : string) =
     let fun get_substitutions_helper(subs_helper_list : string list list, str_helper : string, acc: string list) = 
     case subs_helper_list of
     [] => acc
    | head::subs_list_tail =>
          let 
              val sub = all_except_option(str, head)
          in
              if isSome sub then get_substitutions_helper(subs_list_tail, str_helper,acc@(valOf sub))
              else
                  get_substitutions_helper(subs_list_tail, str_helper, acc)
          end
     in
         get_substitutions_helper(subs_list, str, [])
     end
         
fun similar_names(origin_name_list : string list list, full_name: {first:string,middle:string,last:string}) =
       let 
           val name_list = (#first full_name)::get_substitutions2(origin_name_list, #first full_name)
		   fun similar_names_helper(helper_name_list : string list, helper_full_name : {first:string, middle:string, last:string}) =  
              case helper_name_list of
                   [] => []
                   | name::name_list_tail =>
                             {first=name, last=(#last helper_full_name), middle=(#middle helper_full_name)}::similar_names_helper(name_list_tail,helper_full_name)
       in
         similar_names_helper(name_list,full_name)
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

fun card_color(card_suit : suit, card_rank : rank) = 
    case card_suit of
		Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value(card_suit : suit, card_rank : rank) =
    case card_rank of
        Num i => i
      | Ace => 11
      | _ => 10

fun remove_card(cs : card list, c : card, e) =
    let 
       fun remove_card_helper(cs_helper : card list, c_helper : card, e_helper, acc : card list) = 
            case cs_helper of
      			[] => raise e_helper
              | cs_head::cs_tail =>
                      if cs_head = c_helper 
                      then
                         acc@cs_tail
                      else
                         remove_card_helper(cs_tail,c_helper,e, acc@(cs_head::[]))
     in
        remove_card_helper(cs,c,e,[])
     end
               
