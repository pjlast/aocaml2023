open Base
open Stdio
open Day14

let () =
  print_endline "day 14 part 1";
  let input = In_channel.read_all "inputs/day14.txt" in
  print_endline @@ Int.to_string @@ (input
  |> parse_input
  |> roll_complete
  |> calc_total)
