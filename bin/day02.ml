open Day02

let process_line line =
  let game = Game.of_string_exn line in
  Game.score game

let process_line_2 line =
  let game = Game.of_string_exn line in
  Game.power game

let rec solve_day_2_part_1 ic acc =
  match In_channel.input_line ic with
  | exception End_of_file -> acc
  | None -> acc
  | Some line -> solve_day_2_part_1 ic (acc + process_line line)

let rec solve_day_2_part_2 ic acc =
  match In_channel.input_line ic with
  | exception End_of_file -> acc
  | None -> acc
  | Some line -> solve_day_2_part_2 ic (acc + process_line_2 line)

let () =
  let ic = Stdio.In_channel.create "inputs/day02.txt" in
  print_endline "day 2 part 1";
  print_endline (Int.to_string (solve_day_2_part_1 ic 0));

  let ic = Stdio.In_channel.create "inputs/day02.txt" in
  print_endline "day 2 part 2";
  print_endline (Int.to_string (solve_day_2_part_2 ic 0))
