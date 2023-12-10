open Base
open Stdio

let () =
  let input_list = In_channel.read_all "inputs/day09.txt" in
  let input_list =
    input_list |> String.split_lines |> List.map ~f:(String.split ~on:' ')
  in
  let input_list = List.map input_list ~f:(List.map ~f:Int.of_string) in

  let _ = print_endline "day 9 part 1" in
  let new_entries =
    input_list
    |> List.map ~f:(fun l -> Day09.new_seq_entry @@ Day09.extrapolate l)
  in
  let sum = List.fold new_entries ~init:0 ~f:( + ) in
  print_endline (Int.to_string sum);

  let _ = print_endline "day 9 part 2" in
  let new_entries =
    input_list
    |> List.map ~f:(fun l -> Day09.prev_seq_entry @@ Day09.extrapolate l)
  in
  let sum = List.fold new_entries ~init:0 ~f:( + ) in
  print_endline (Int.to_string sum)
