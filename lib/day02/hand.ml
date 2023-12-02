open Base

type t = { red : int; green : int; blue : int }

(* We expect a string of the form "2 red, 4 green, 7 blue" *)
let of_string line =
  let parts =
    List.map (String.split line ~on:',') ~f:(fun s -> String.strip s)
  in

  let red =
    List.find_map parts ~f:(fun s ->
        match String.chop_suffix ~suffix:" red" s with
        | Some num -> Some (Int.of_string num)
        | None -> None)
  in

  let blue =
    List.find_map parts ~f:(fun s ->
        match String.chop_suffix ~suffix:" blue" s with
        | Some num -> Some (Int.of_string num)
        | None -> None)
  in

  let green =
    List.find_map parts ~f:(fun s ->
        match String.chop_suffix ~suffix:" green" s with
        | Some num -> Some (Int.of_string num)
        | None -> None)
  in

  {
    red = Option.value red ~default:0;
    green = Option.value green ~default:0;
    blue = Option.value blue ~default:0;
  }

let%test _ =
  match of_string "2 red, 4 green, 7 blue" with
  | { red = 2; green = 4; blue = 7 } -> true
  | _ -> false

let%test _ =
  match of_string "1 green" with
  | { red = 0; green = 1; blue = 0 } -> true
  | _ -> false
