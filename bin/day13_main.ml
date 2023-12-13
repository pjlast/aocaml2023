open Base
open Stdio

let () =
  print_endline "day 13 part 1";
  print_endline
    (Int.to_string
       (In_channel.read_all "inputs/day13.txt" |> Day13.process_big_input))
