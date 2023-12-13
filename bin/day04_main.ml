open Day04
open Base
open Stdio

let solve_part_1 line =
  let parts = card_of_string line in
  let win_nums = List.hd_exn parts in
  let scratch_nums = List.last_exn parts in
  List.fold scratch_nums ~init:0 ~f:(fun acc num ->
      match List.find win_nums ~f:(fun e -> e = num) with
      | Some _ ->
          if acc = 0 then
            1
          else
            acc * 2
      | None -> acc)

let rec solve_day_4_part_1 ic acc =
  let input = Stdio.In_channel.input_line ic in
  match input with
  | exception End_of_file -> acc
  | None -> acc
  | Some line -> solve_day_4_part_1 ic (acc + solve_part_1 line)

let tbl : (int, int) Hashtbl.t = Hashtbl.create (module Int)

let incr_tbl tbl index num =
  let curr = Option.value (Hashtbl.find tbl index) ~default:1 in
  let _ = Hashtbl.remove tbl index in
  Hashtbl.add_exn tbl ~key:index ~data:(curr + num)

let solve_part_2 card_num line =
  let parts = card_of_string line in
  let matches = count_matches parts in
  let count =
    match Hashtbl.find tbl card_num with
    | Some v -> v
    | None ->
        Hashtbl.add_exn tbl ~key:card_num ~data:1;
        1
  in
  for i = card_num + 1 to card_num + matches do
    incr_tbl tbl i count
  done;
  ()

let rec solve_day_4_part_2 ic card_num =
  let input = Stdio.In_channel.input_line ic in
  match input with
  | exception End_of_file -> ()
  | None -> ()
  | Some line ->
      solve_part_2 card_num line;
      solve_day_4_part_2 ic (card_num + 1)

let () =
  let ic = Stdio.In_channel.create "inputs/day04.txt" in
  print_endline "day 4 part 1";
  print_endline (Int.to_string (solve_day_4_part_1 ic 0));

  let ic = Stdio.In_channel.create "inputs/day04.txt" in
  print_endline "day 4 part 2";
  solve_day_4_part_2 ic 1;

  print_endline
    (Int.to_string (tbl |> Hashtbl.data |> List.fold ~init:0 ~f:( + )))
