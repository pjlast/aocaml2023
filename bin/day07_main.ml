open Base
open Day07

let input =
  Stdio.In_channel.create "inputs/day07.txt" |> Stdio.In_channel.input_all

let processed_list =
  let x =
    input |> String.split_lines |> List.map ~f:parse_line
    |> List.sort ~compare:(fun (ac, _) (bc, _) -> compare_hands ac bc)
  in
  let total =
    List.foldi x ~init:0 ~f:(fun index acc (_, bet) ->
        acc + ((index + 1) * bet))
  in
  total

let () =
  Stdio.print_endline "day 7 part 1";
  Stdio.print_endline (Int.to_string (processed_list));

  Stdio.print_endline "day 7 part 2";
  Stdio.print_endline "TODO"
