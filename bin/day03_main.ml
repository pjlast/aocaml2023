open Day03
open Core

let solve_day_3_part_1 ic =
  let input = Stdio.In_channel.input_all ic in
  let symmap = symbol_list_of_string input in
  let nummap = number_list_of_string input in
  sum_valid_nums nummap symmap

let solve_day_3_part_2 ic =
  let input = Stdio.In_channel.input_all ic in
  let gearmap = gear_list_of_string input in
  let nummap = number_list_of_string input in
  let valid_vals = nums_adjacent_gears gearmap nummap in
  List.fold
    (List.fold valid_vals ~init:[] ~f:(fun acc valid_val ->
         acc @ [ List.fold valid_val ~init:1 ~f:( * ) ]))
    ~init:0 ~f:( + )

let () =
  let ic = Stdio.In_channel.create "inputs/day03.txt" in
  print_endline "day 3 part 1";
  print_endline (Int.to_string (solve_day_3_part_1 ic));

  let ic = Stdio.In_channel.create "inputs/day03.txt" in
  print_endline "day 3 part 2";
  print_endline (Int.to_string (solve_day_3_part_2 ic))
