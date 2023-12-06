open Base
open Day05
open Stdio

let seeds_1 =
  let seed_input =
    Stdio.In_channel.(
      create "inputs/day05_seeds.txt" |> input_all |> String.strip)
  in
  seed_input |> String.split ~on:' ' |> List.map ~f:Int.of_string

let dsm_of_string str =
  str |> String.split ~on:' ' |> List.map ~f:Int.of_string |> function
  | [ d; s; r ] -> { dest_start = d; source_start = s; range = r }
  | _ -> failwith "Invalid map"

let read_input_map source =
  let rec read_input_map' ic acc =
    match Stdio.In_channel.input_line ic with
    | exception End_of_file -> acc
    | None -> acc
    | Some line -> read_input_map' ic (acc @ [ dsm_of_string line ])
  in
  let ic = Stdio.In_channel.create source in
  read_input_map' ic []

let solve_day_5_part_1 =
  let maps =
    [
      read_input_map "inputs/day05_seed_to_soil.txt";
      read_input_map "inputs/day05_soil_to_fertilizer.txt";
      read_input_map "inputs/day05_fertilizer_to_water.txt";
      read_input_map "inputs/day05_water_to_light.txt";
      read_input_map "inputs/day05_light_to_temperature.txt";
      read_input_map "inputs/day05_temperature_to_humidity.txt";
      read_input_map "inputs/day05_humidity_to_location.txt";
    ]
  in
  let res_l =
    List.map seeds_1 ~f:(fun seed ->
        List.fold maps ~init:seed ~f:(fun acc map ->
            Day05.map_list_source_to_dest map acc))
  in
  Option.value_exn (List.min_elt ~compare:(fun a b -> a - b) res_l)

(* PART 2 *)

let seeds_2 =
  let seed_input =
    Stdio.In_channel.(
      create "inputs/day05_seeds.txt" |> input_all |> String.strip)
  in
  let seeds = seed_input |> String.split ~on:' ' |> List.map ~f:Int.of_string in
  let rec expand_seeds seeds acc =
    match seeds with
    | [] -> acc
    | start :: range :: t -> expand_seeds t (acc @ [ (start, range) ])
    | _ -> acc
  in

  expand_seeds seeds []

let get_location maps seed =
  List.fold maps ~init:seed ~f:(fun acc map ->
      Day05.map_list_source_to_dest map acc)

let solve_day_5_part_2 =
  let maps =
    [
      read_input_map "inputs/day05_seed_to_soil.txt";
      read_input_map "inputs/day05_soil_to_fertilizer.txt";
      read_input_map "inputs/day05_fertilizer_to_water.txt";
      read_input_map "inputs/day05_water_to_light.txt";
      read_input_map "inputs/day05_light_to_temperature.txt";
      read_input_map "inputs/day05_temperature_to_humidity.txt";
      read_input_map "inputs/day05_humidity_to_location.txt";
    ]
  in
  let res_l =
    List.map seeds_2 ~f:(fun (start, range) ->
        let seq = Sequence.range start (start + range) in
        let seq = Sequence.map seq ~f:(get_location maps) in
        Option.value_exn (Sequence.min_elt seq ~compare:(fun a b -> a - b)))
  in
  Option.value_exn (List.min_elt ~compare:(fun a b -> a - b) res_l)

let () =
  print_endline "day 5 part 1";
  print_endline (Int.to_string solve_day_5_part_1);

  print_endline "day 5 part 2";
  print_endline (Int.to_string solve_day_5_part_2)
