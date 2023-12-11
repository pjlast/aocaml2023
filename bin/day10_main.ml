open Base
open Stdio
open Day10

let () =
  print_endline "day10 part 1";
  let input = In_channel.read_all "inputs/day10.txt" in
  let map = input |> map_of_input in
  let start = find_start map in
  let starty, startx = start in
  let len = get_loop_length (starty, startx) (starty + 1, startx) map in
  print_endline (Int.to_string ((len + 1) / 2))
