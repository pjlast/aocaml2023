open Base

(* PART 1 *)
let int_list_of_string str =
  str |> String.strip |> String.split ~on:' '
  |> List.filter ~f:(fun i -> not (i |> String.strip |> String.is_empty))
  |> List.map ~f:Int.of_string

let card_of_string str =
  String.split ~on:':' str |> List.last |> Option.value_exn
  |> String.split ~on:'|'
  |> List.map ~f:int_list_of_string

(* PART 2 *)

let count_matches cards =
  let win_nums = List.hd_exn cards in
  let scratch_nums = List.last_exn cards in
  List.fold scratch_nums ~init:0 ~f:(fun acc num ->
      match List.find win_nums ~f:(fun e -> e = num) with
      | Some _ -> acc + 1
      | None -> acc)
