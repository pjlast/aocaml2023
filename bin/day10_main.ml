open Base
open Stdio
open Day10

let process_pos prev prev_c c =
  match (c, prev_c, prev) with
  | '|', _, 'O' -> ('I', '.')
  | '|', _, 'I' -> ('O', '.')
  | 'L', _, _ -> (prev, 'L')
  | 'F', _, _ -> (prev, 'F')
  | '7', 'L', 'O' -> ('I', '.')
  | '7', 'L', 'I' -> ('O', '.')
  | '7', 'F', 'O' -> ('O', '.')
  | '7', 'F', 'I' -> ('I', '.')
  | 'J', 'F', 'O' -> ('I', '.')
  | 'J', 'F', 'I' -> ('O', '.')
  | 'J', 'L', 'O' -> ('O', '.')
  | 'J', 'L', 'I' -> ('I', '.')
  | _, _, _ -> (prev, prev_c)

let process_line line =
  let rec process_line' line acc prev prev_c =
    match line with
    | h :: t -> (
        let next_prev, next_prev_c = process_pos prev prev_c h in
        match h with
        | '.' -> process_line' t (acc @ [ prev ]) next_prev next_prev_c
        | c -> process_line' t (acc @ [ c ]) next_prev next_prev_c)
    | [] -> acc
  in
  let new_line = process_line' line [] 'O' '.' in
  Stdio.print_endline (String.of_char_list new_line);
  String.of_char_list new_line |> String.count ~f:(Char.equal 'I')

let () =
  print_endline "day10 part 1";
  let input = In_channel.read_all "inputs/day10.txt" in
  let map = input |> map_of_input in
  let start = find_start map in
  let starty, startx = start in
  let len = get_loop_length (starty, startx) (starty + 1, startx) map in
  print_endline (Int.to_string ((len + 1) / 2));

  print_endline "day10 part 1";
  let input = In_channel.read_all "inputs/day10_processed.txt" in
  let map = input |> String.split_lines |> List.map ~f:String.to_list in
  let counts = map |> List.map ~f:process_line in
  let total = List.fold counts ~init:0 ~f:( + ) in
  Stdio.print_endline (Int.to_string total)
