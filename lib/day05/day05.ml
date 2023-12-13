open Base

type dest_source_map = { dest_start : int; source_start : int; range : int }

let map_source_to_dest dsm i =
  if i >= dsm.source_start && i < dsm.source_start + dsm.range then
    Some (i + dsm.dest_start - dsm.source_start)
  else
    None

let%test _ =
  let dsm = { dest_start = 50; source_start = 98; range = 2 } in
  Option.value_exn (Some 51) = Option.value_exn (map_source_to_dest dsm 99)

let%test _ =
  let dsm = { dest_start = 52; source_start = 50; range = 48 } in
  Option.value_exn (Some 55) = Option.value_exn (map_source_to_dest dsm 53)

let map_list_source_to_dest dsm_l i =
  match List.find_map dsm_l ~f:(fun dsm -> map_source_to_dest dsm i) with
  | Some dest -> dest
  | None -> i

let%test _ =
  let dsm_l =
    [
      { dest_start = 50; source_start = 98; range = 2 };
      { dest_start = 52; source_start = 50; range = 48 };
    ]
  in
  let mlstd = map_list_source_to_dest dsm_l in
  51 = mlstd 99 && 55 = mlstd 53 && 10 = mlstd 10
