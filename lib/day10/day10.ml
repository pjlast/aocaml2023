open Base

let test_input = {|.....
.S-7.
.|.|.
.L-J.
.....|}

let map_of_input : string -> char list list =
 fun input -> input |> String.split_lines |> List.map ~f:String.to_list

let find_start map =
  List.find_mapi map ~f:(fun i row ->
      let found =
        List.find_mapi row ~f:(fun j col ->
            if col |> Char.equal 'S' then
              Some j
            else
              None)
      in
      match found with
      | Some j -> Some (i, j)
      | None -> None)
  |> Option.value_exn

let%test _ =
  let s = test_input |> map_of_input |> find_start in
  s |> Poly.( = ) (1, 1)

let get_char_at (row, col) map =
  List.find_mapi map ~f:(fun i r ->
      if i = row then
        List.find_mapi r ~f:(fun j c ->
            if j = col then
              Some c
            else
              None)
      else
        None)
  |> Option.value_exn

let%test _ =
  let c = test_input |> map_of_input |> get_char_at (1, 3) in
  c |> Char.equal '7'

(** [take_step map start dest] takes a step on [map] at from position [start] to
    position [dest] and returns the next step to take. *)
let take_step map start dest =
  let starty, startx = start in
  let desty, destx = dest in
  let c = get_char_at dest map in
  match c with
  | '|' ->
      if starty < desty then
        (starty + 2, startx)
      else
        (starty - 2, startx)
  | '-' ->
      if startx < destx then
        (starty, startx + 2)
      else
        (starty, startx - 2)
  | 'L' ->
      if starty < desty then
        (starty + 1, startx + 1)
      else
        (starty - 1, startx - 1)
  | 'J' ->
      if starty < desty then
        (starty + 1, startx - 1)
      else
        (starty - 1, startx + 1)
  | '7' ->
      if startx < destx then
        (starty + 1, startx + 1)
      else
        (starty - 1, startx - 1)
  | 'F' ->
      if starty > desty then
        (starty - 1, startx + 1)
      else
        (starty + 1, startx - 1)
  | _ -> failwith "invalid move"

let%test _ =
  let map = test_input |> map_of_input in
  let next = take_step map (1, 2) (1, 3) in
  next |> Poly.( = ) (2, 3)

let get_loop_length start dest map =
  let rec aux start dest acc =
    let c = get_char_at dest map in
    match c with
    | 'S' -> acc
    | _ -> aux dest (take_step map start dest) (acc + 1)
  in
  aux start dest 0

let%test _ =
  let map = test_input |> map_of_input in
  let len = get_loop_length (1, 1) (1, 2) map in
  7 = len
