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
                case sub of
                   SOME sub_val => sub_val@get_substitutions1(subs_list_tail, str) 
                 | NONE =>get_substitutions1(subs_list_tail, str)
            end

fun get_substitutions2(subs_list : string list list, str : string) =
     let fun get_substitutions_helper(subs_helper_list : string list list, str_helper : string, acc: string list) = 
     case subs_helper_list of
     [] => acc
    | head::subs_list_tail =>
          let 
              val sub = all_except_option(str, head)
          in
              case sub of
                 SOME sub_val => get_substitutions_helper(subs_list_tail, str_helper,acc@sub_val)
                | NONE => get_substitutions_helper(subs_list_tail, str_helper, acc)
          end
     in
         get_substitutions_helper(subs_list, str, [])
     end
         
fun similar_names(origin_name_list : string list list, full_name: {first:string,middle:string,last:string}) =
       case full_name of
           {first=first_name, middle=middle_name, last = last_name} => 
           let 
               val name_list = first_name::get_substitutions2(origin_name_list, first_name)
		       fun similar_names_helper(helper_name_list : string list) =  
                  case helper_name_list of
                       [] => []
                       | name::name_list_tail =>
                             {first=name, last=last_name, middle=middle_name}::similar_names_helper(name_list_tail)
           in
              similar_names_helper(name_list)
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

fun all_same_color(cs : card list) =
    case cs of
       [] => true
       | c::[] => true
       | c1::c2::[] => card_color(c1) = card_color(c2)
       | c1::c2::cs_tail =>
          if card_color(c1) = card_color(c2)
          then
              all_same_color(c2::cs_tail)
          else
              false

fun sum_cards(cs : card list) = 
     let 
       fun sum_cards_helper(cs_helper: card list, acc : int) = 
          case cs_helper of 
              [] => acc
             | xs::tail =>  sum_cards_helper(tail, acc + card_value(xs) )
     in
       sum_cards_helper(cs, 0)
     end

fun score(cs : card list, goal : int) = 
	let 
        val init_sum = sum_cards(cs)
        val same = all_same_color(cs)
	in
      if init_sum > goal
      then
          if same then 3*(init_sum - goal) div 2 else 3*(init_sum-goal)
      else 
         if same then (goal - init_sum) div 2 else (goal -init_sum)
    end

fun officiate(cs : card list, mv : move list, goal : int) = 
    let
       fun officiate_helper(cs_helper : card list, mv_helper: move list, held_cards : card list, goal_helper : int) =
           case mv_helper of
               [] => score(held_cards, goal_helper)
             | m::mv_tail =>
                 case m of
                     Discard(c) => officiate_helper(cs_helper, mv_tail, remove_card(held_cards,c,IllegalMove ),goal_helper)
                    | Draw =>
                          case cs_helper of
							 [] => score(held_cards, goal_helper) 
                            | cs_hd::cs_helper_tail => 
                                let 
                                    val sum_value = sum_cards(cs_hd::held_cards)
                                in
                                   if sum_value > goal_helper 
                                   then
                                      score(cs_hd::held_cards, goal_helper)
                               else
                                  officiate_helper(cs_helper_tail, mv_tail, cs_hd::held_cards, goal_helper)
                              end

                     
                        
                  
    in
        officiate_helper(cs,mv,[],goal)
    end
                   
       
           
