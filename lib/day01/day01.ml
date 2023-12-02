open Base

(* PART 1 *)

let rec first_num_exn s =
  match String.to_list s with
  | [] -> failwith "No number found"
  | h :: _ when Char.is_digit h -> h |> String.of_char |> Int.of_string
  | _ :: t -> first_num_exn (String.of_char_list t)

let%test _ = first_num_exn "1abc2" = 1
let%test _ = first_num_exn "pqr3stu8vwx" = 3
let%test _ = first_num_exn "treb7uchet" = 7

let rec last_num_exn s =
  match List.rev (String.to_list s) with
  | [] -> failwith "Non number found"
  | h :: _ when Char.is_digit h -> h |> String.of_char |> Int.of_string
  | _ :: t -> last_num_exn (String.of_char_list (List.rev t))

let%test _ = last_num_exn "1abc2" = 2
let%test _ = last_num_exn "pqr3stu8vwx" = 8
let%test _ = last_num_exn "treb7uchet" = 7

(* PART 2 *)

let rec first_num_2_exn s =
  match String.to_list s with
  | [] -> failwith "No number found"
  | 'o' :: 'n' :: 'e' :: _ -> 1
  | 't' :: 'w' :: 'o' :: _ -> 2
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5
  | 's' :: 'i' :: 'x' :: _ -> 6
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9
  | c :: _ when Char.is_digit c -> c |> Char.to_string |> Int.of_string
  | _ :: t -> first_num_2_exn (String.of_char_list t)

let%test _ = first_num_2_exn "two1nine" = 2
let%test _ = first_num_2_exn "abcone2threexyz" = 1
let%test _ = first_num_2_exn "7pqrstsixteen" = 7

let rec last_num_2_exn s =
  match List.rev (String.to_list s) with
  | [] -> failwith "No number found"
  | 'e' :: 'n' :: 'o' :: _ -> 1
  | 'o' :: 'w' :: 't' :: _ -> 2
  | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> 3
  | 'r' :: 'u' :: 'o' :: 'f' :: _ -> 4
  | 'e' :: 'v' :: 'i' :: 'f' :: _ -> 5
  | 'x' :: 'i' :: 's' :: _ -> 6
  | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> 7
  | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> 8
  | 'e' :: 'n' :: 'i' :: 'n' :: _ -> 9
  | c :: _ when Char.is_digit c -> c |> Char.to_string |> Int.of_string
  | _ :: t -> last_num_2_exn (String.of_char_list (List.rev t))

let%test _ = last_num_2_exn "two1nine" = 9
let%test _ = last_num_2_exn "abcone2threexyz" = 3
let%test _ = last_num_2_exn "7pqrstsixteen" = 6
