open Base
open Stdio

let hash str =
  let open List.Monad_infix in
  str
  |> String.to_list
  >>| Char.to_int
  |> List.fold ~init:0 ~f:(fun acc c -> (acc + c) * 17 % 256)

let () =
  let open List.Monad_infix in
  print_endline "day 15 part 1";
  let input = String.strip (In_channel.read_all "inputs/day15.txt") in
  let steps = String.split input ~on:',' in
  let res = steps >>| hash |> List.fold ~init:0 ~f:( + ) in
  print_endline (Int.to_string res)
