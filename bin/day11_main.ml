open Base
open Stdio
open Day11

let () =
  print_endline "day11 part 1";
  let input = In_channel.read_all "inputs/day11.txt" in
  let map = input |> char_list_of_string |> expand_galaxy in
  let galaxies = map |> get_galaxies in
  let dists = map_pairs galaxies ~f:dist in
  let sum = List.fold dists ~init:0 ~f:( + ) in
  print_endline (Int.to_string sum);

  print_endline "day11 part 2";
  let input = In_channel.read_all "inputs/day11.txt" in
  let map = input |> char_list_of_string |> expand_galaxy_million in
  let galaxies = map |> get_galaxies_2 in
  let dists = map_pairs galaxies ~f:dist in
  let sum = List.fold dists ~init:0 ~f:( + ) in
  print_endline (Int.to_string sum)
