open Base

type hand =
  | Five_of_a_kind of int list
  | Four_of_a_kind of int list
  | Full_house of int list
  | Three_of_a_kind of int list
  | Two_pair of int list
  | One_pair of int list
  | High_card of int list

let parse_line str =
  match String.split ~on:' ' str with
  | [ hand; bet ] -> (hand, Int.of_string bet)
  | _ -> failwith "Invalid hand"

let%test _ =
  match parse_line "32T3K 765" with "32T3K", 765 -> true | _ -> false

let card_to_num = function
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | c when Char.is_digit c -> c |> String.of_char |> Int.of_string
  | _ -> failwith "Invalid card"

let parse_hand_type str =
  let card_list = str |> String.to_list |> List.map ~f:card_to_num in
  let grouped =
    List.group
      ~break:(fun a b -> not (a = b))
      (List.sort ~compare:( - ) card_list)
  in
  match grouped |> List.map ~f:List.length |> List.sort ~compare:( - ) with
  | [ 5 ] -> Five_of_a_kind card_list
  | [ 1; 4 ] -> Four_of_a_kind card_list
  | [ 2; 3 ] -> Full_house card_list
  | [ 1; 1; 3 ] -> Three_of_a_kind card_list
  | [ 1; 2; 2 ] -> Two_pair card_list
  | [ 1; 1; 1; 2 ] -> One_pair card_list
  | [ 1; 1; 1; 1; 1 ] -> High_card card_list
  | _ -> failwith "Invalid hand"

let%test _ =
  match parse_hand_type "AAAAA" with Five_of_a_kind _ -> true | _ -> false

let%test _ =
  match parse_hand_type "23456" with High_card _ -> true | _ -> false

let hand_score = function
  | Five_of_a_kind _ -> 7
  | Four_of_a_kind _ -> 6
  | Full_house _ -> 5
  | Three_of_a_kind _ -> 4
  | Two_pair _ -> 3
  | One_pair _ -> 2
  | High_card _ -> 1

let rec compare_high_card (a : int list) (b : int list) =
  match (a, b) with
  | ah :: at, bh :: bt ->
      if ah - bh = 0 then compare_high_card at bt else ah - bh
  | [], [] -> 0
  | _ -> failwith "Invalid hands"

let compare_hands a b =
  let hand_a = parse_hand_type a in
  let hand_b = parse_hand_type b in
  if hand_score hand_a = hand_score hand_b then
    let hand_num_a = a |> String.to_list |> List.map ~f:card_to_num in
    let hand_num_b = b |> String.to_list |> List.map ~f:card_to_num in
    compare_high_card hand_num_a hand_num_b
  else hand_score hand_a - hand_score hand_b

let%test _ = compare_hands "AAAAA" "KKKKK" > 0
let%test _ = compare_hands "KKKKK" "AAAAA" < 0
let%test _ = compare_hands "KKKKK" "KKKKK" = 0
let%test _ = compare_hands "23456" "QQJKK" < 0

let input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let%test _ =
  let processed_list =
    input |> String.split_lines |> List.map ~f:parse_line
    |> List.sort ~compare:(fun (ac, _) (bc, _) -> compare_hands ac bc)
  in
  let total =
    List.foldi processed_list ~init:0 ~f:(fun index acc (_, bet) ->
        acc + ((index + 1) * bet))
  in
  total = 6440

(* PART 2 *)
let card_to_num_2 = function
  | 'T' -> 10
  | 'J' -> 1
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | c when Char.is_digit c -> c |> String.of_char |> Int.of_string
  | _ -> failwith "Invalid card"
