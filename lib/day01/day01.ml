open Base

(* PART 1 *)

let first_num_exn s =
  s
  |> String.to_list
  |> List.find ~f:Char.is_digit
  |> Option.value_exn
  |> String.of_char
  |> Int.of_string

let%test _ = first_num_exn "1abc2" = 1
let%test _ = first_num_exn "pqr3stu8vwx" = 3
let%test _ = first_num_exn "treb7uchet" = 7
let last_num_exn s = first_num_exn (String.rev s)
let%test _ = last_num_exn "1abc2" = 2
let%test _ = last_num_exn "pqr3stu8vwx" = 8
let%test _ = last_num_exn "treb7uchet" = 7

(* PART 2 *)

let is_prefixes s ~prefixes =
  List.find prefixes ~f:(fun prefix -> String.is_prefix s ~prefix)
  |> Option.is_some

let rec first_num_2_exn s acc =
  match s with
  | "" -> acc
  | s when is_prefixes s ~prefixes:[ "one"; "1" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 1 ])
  | s when is_prefixes s ~prefixes:[ "two"; "2" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 2 ])
  | s when is_prefixes s ~prefixes:[ "three"; "3" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 3 ])
  | s when is_prefixes s ~prefixes:[ "four"; "4" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 4 ])
  | s when is_prefixes s ~prefixes:[ "five"; "5" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 5 ])
  | s when is_prefixes s ~prefixes:[ "six"; "6" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 6 ])
  | s when is_prefixes s ~prefixes:[ "seven"; "7" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 7 ])
  | s when is_prefixes s ~prefixes:[ "eight"; "8" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 8 ])
  | s when is_prefixes s ~prefixes:[ "nine"; "9" ] ->
      first_num_2_exn (String.drop_prefix s 1) (acc @ [ 9 ])
  | _ -> first_num_2_exn (String.drop_prefix s 1) acc

let%test _ = List.hd_exn @@ first_num_2_exn "two1nine" [] = 2
let%test _ = List.hd_exn @@ first_num_2_exn "abcone2threexyz" [] = 1
let%test _ = List.hd_exn @@ first_num_2_exn "7pqrstsixteen" [] = 7
let%test _ = List.last_exn @@ first_num_2_exn "two1nine" [] = 9
let%test _ = List.last_exn @@ first_num_2_exn "abcone2threexyz" [] = 3
let%test _ = List.last_exn @@ first_num_2_exn "7pqrstsixteen" [] = 6
