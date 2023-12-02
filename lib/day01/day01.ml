open Base

(* PART 1 *)

let first_num_exn s =
  s |> String.to_list |> List.find ~f:Char.is_digit |> Option.value_exn
  |> String.of_char |> Int.of_string

let%test _ = first_num_exn "1abc2" = 1
let%test _ = first_num_exn "pqr3stu8vwx" = 3
let%test _ = first_num_exn "treb7uchet" = 7
let last_num_exn s = first_num_exn (String.rev s)
let%test _ = last_num_exn "1abc2" = 2
let%test _ = last_num_exn "pqr3stu8vwx" = 8
let%test _ = last_num_exn "treb7uchet" = 7

(* PART 2 *)

let number_map =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
  ]

let rec first_num_2_exn s =
  let value =
    List.find_map number_map ~f:(fun (num, value) ->
        if String.is_prefix s ~prefix:num then Some value else None)
  in
  match value with
  | Some v -> v
  | None -> first_num_2_exn (String.drop_prefix s 1)

let%test _ = first_num_2_exn "two1nine" = 2
let%test _ = first_num_2_exn "abcone2threexyz" = 1
let%test _ = first_num_2_exn "7pqrstsixteen" = 7

let rec last_num_2_exn s =
  let value =
    List.find_map number_map ~f:(fun (num, value) ->
        if String.is_suffix s ~suffix:num then Some value else None)
  in
  match value with
  | Some v -> v
  | None -> last_num_2_exn (String.drop_suffix s 1)

let%test _ = last_num_2_exn "two1nine" = 9
let%test _ = last_num_2_exn "abcone2threexyz" = 3
let%test _ = last_num_2_exn "7pqrstsixteen" = 6
