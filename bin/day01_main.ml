open Day01

let rec solve_day_1_part_1 ic acc =
  match Stdio.In_channel.input_line ic with
  | exception End_of_file -> acc
  | None -> acc
  | Some line ->
      solve_day_1_part_1 ic (acc + (10 * first_num_exn line) + last_num_exn line)

let rec solve_day_1_part_2 ic acc =
  match Stdio.In_channel.input_line ic with
  | exception End_of_file -> acc
  | None -> acc
  | Some line ->
      solve_day_1_part_2 ic
        (acc + (10 * first_num_2_exn line) + last_num_2_exn line)

let () =
  let ic = Stdio.In_channel.create "inputs/day01.txt" in
  print_endline "day 1 part 1";
  print_endline (Int.to_string (solve_day_1_part_1 ic 0));

  let ic = Stdio.In_channel.create "inputs/day01.txt" in
  print_endline "day 1 part 2";
  print_endline (Int.to_string (solve_day_1_part_2 ic 0))
