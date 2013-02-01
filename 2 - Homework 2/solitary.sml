(* HOMEWORK 2 *)
(* Pre-defined types and functions *)
datatype suit  = Clubs | Diamonds | Hearts | Spades
datatype rank  = Jack | Queen | King | Ace | Num of int
datatype color = Red | Black

type card = suit * rank
datatype move = Discard of card | Draw

exception IllegalMove

fun same_string (s1 : string, s2 : string) = s1 = s2

(* Problem 1 *)
fun all_except_option (_, [ ]) = NONE
  | all_except_option (str, x :: xs) =
        if same_string (str, x) then
          SOME xs
        else
          case all_except_option (str, xs) of
               NONE   => NONE
             | SOME y => SOME (x :: y)

fun get_substitutions1 ([ ], _) = [ ]
  | get_substitutions1 (x :: xs, str) =
      case all_except_option (str, x) of
           NONE   => get_substitutions1 (xs, str)
         | SOME y => y @ get_substitutions1 (xs, str)

fun get_substitutions2 (lst, str) =
  let
    fun iter ([ ], acc) = acc
      | iter (x :: xs, acc) =
          case all_except_option (str, x) of
               NONE   => iter (xs, acc)
             | SOME y => iter (xs, y @ acc)
  in
    iter(lst, [ ])
  end

fun similar_names (lst, { first : string, middle : string, last : string}) =
  let
    fun iter ([ ], acc) = acc
      | iter (x :: xs, acc) =
          iter(xs, { first = x, middle = middle, last = last } :: acc)
  in
    iter(first :: get_substitutions2 (lst, first), [ ])
  end

(* Problem 2 *)
fun card_color (Clubs, _)    = Black
  | card_color (Spades, _)   = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _)   = Red

fun card_value (_, Ace)   = 11
  | card_value (_, Num n) = n
  | card_value (_, _)     = 10

fun remove_card ([ ], _, e : exn) = raise e
  | remove_card (card :: cs, c, e : exn) =
      if card = c then
        cs
      else
        card :: remove_card(cs, c, e)

fun all_same_color ([ ]) = true
  | all_same_color (_ :: [ ]) = true
  | all_same_color (x :: y :: ys) =
      card_color x = card_color y andalso all_same_color(y :: ys)

fun sum_cards cards =
  let
    fun iter ([ ], acc) = acc
      | iter (x :: xs, acc) = iter(xs, acc + card_value x)
  in
    iter(cards, 0)
  end

fun score (cards, goal) =
  let
    val sum = sum_cards cards
    val pre = if sum > goal then 3 * (sum - goal) else (goal - sum)
  in
    if all_same_color cards then pre div 2 else pre
  end

fun officiate (cards, moves, goal) =
  let
    fun play (_, [ ], held) = score (held, goal)
      | play ([ ], Draw :: _, held) = score (held, goal)
      | play (card :: cards, Draw :: moves, held) =
          if sum_cards held + card_value card > goal then
            score (card :: held, goal)
          else
            play (cards, moves, card :: held)
      | play (_ :: cards, Discard x :: moves, held) =
          play (cards, moves, remove_card (held, x, IllegalMove))
  in
    play(cards, moves, [ ])
  end

