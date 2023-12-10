open Base

let zip_map : 'a list -> f:('a -> 'a -> 'b) -> 'b list =
 fun list ~f ->
  let rec aux list acc =
    match list with
    | [] -> acc
    | _ :: [] -> acc
    | h1 :: h2 :: t -> aux ([ h2 ] @ t) (acc @ [ f h1 h2 ])
  in
  aux list []

let%test _ =
  let open Poly in
  zip_map [ 1; 2; 3; 4 ] ~f:(fun a b -> b - a) = [ 1; 1; 1 ]

let extrapolate : int list -> int list list =
 fun list ->
  let rec aux list' acc =
    let diffs = zip_map list' ~f:(fun a b -> b - a) in
    if Option.is_some (List.all_equal diffs ~equal:(fun a _ -> a = 0)) then
      [ diffs ] @ acc @ [ list ]
    else
      aux diffs ([ diffs ] @ acc)
  in

  aux list []

let%test _ =
  let open Poly in
  extrapolate [ 1; 2; 4; 7 ] = [ [ 0 ]; [ 1; 1 ]; [ 1; 2; 3 ]; [ 1; 2; 4; 7 ] ]

let new_seq_entry : int list list -> int =
 fun list -> List.fold list ~init:0 ~f:(fun acc l -> List.last_exn l + acc)

let%test _ = new_seq_entry @@ extrapolate [ 1; 2; 4; 7 ] = 11

let prev_seq_entry : int list list -> int =
 fun list -> List.fold list ~init:0 ~f:(fun acc l -> List.hd_exn l - acc)

let%test _ = prev_seq_entry @@ extrapolate [ 10; 13; 16; 21; 30; 45 ] = 5
