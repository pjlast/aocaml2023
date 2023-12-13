open Base

let input1 =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.|}

let input2 =
  {|#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}

let total_input =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map xss ~f:List.hd_exn)
      :: transpose (xs :: List.map xss ~f:List.tl_exn)

let process_input input =
  input |> String.split_lines |> List.map ~f:String.to_list

let comp_lists l1 l2 =
  let l1 =
    if List.length l1 > List.length l2 then
      List.take l1 (List.length l2)
    else
      l1
  in
  let l2 =
    if List.length l2 > List.length l1 then
      List.take l2 (List.length l1)
    else
      l2
  in
  Poly.( = ) l1 l2

let%test _ = comp_lists [ [ 1; 2 ]; [ 3; 4 ] ] [ [ 1; 2 ] ]
let%test _ = comp_lists [ [ 1; 2 ] ] [ [ 1; 2 ]; [ 3; 4 ] ]
let%test _ = not (comp_lists [ [ 2; 2 ] ] [ [ 1; 2 ]; [ 3; 4 ] ])

let rec check_input_first input n =
  let first, second = List.split_n input n in
  if List.is_empty second then
    None
  else
    let first = List.rev first in
    if comp_lists first second then
      Some n
    else
      check_input_first input (n + 1)

let%test _ =
  match check_input_first (process_input input2) 1 with
  | Some n -> n = 4
  | None -> false

let check_input input =
  let processed_input = input |> process_input in
  let num =
    match check_input_first processed_input 1 with
    | Some n -> n * 100
    | None ->
        let transposed_input = processed_input |> transpose in
        Option.value_exn (check_input_first transposed_input 1)
  in
  num

let check_processed_input processed_input =
  let num =
    match check_input_first processed_input 1 with
    | Some n -> n * 100
    | None ->
        let transposed_input = processed_input |> transpose in
        Option.value_exn (check_input_first transposed_input 1)
  in
  num

let%test _ = check_input input1 = 5
let%test _ = check_input input2 = 400

let process_big_input input =
  input
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> List.group ~break:(fun l _ -> List.is_empty l)
  |> List.map ~f:(fun processed_input ->
         processed_input
         |> List.filter ~f:(fun line -> not (Poly.( = ) line []))
         |> check_processed_input)
  |> List.fold ~init:0 ~f:( + )

let%test _ = process_big_input total_input = 405
