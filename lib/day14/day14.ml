open Base

let input =
  {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....|}

type spot = Rock | Stop | Empty

let spot_of_char c =
  match c with
  | 'O' -> Rock
  | '#' -> Stop
  | _ -> Empty

let char_of_spot s =
  match s with
  | Rock -> 'O'
  | Stop -> '#'
  | Empty -> '.'

let roll_up line1 line2 =
  for i = 0 to Array.length line1 - 1 do
    if line2.(i) |> Poly.( = ) Rock && line1.(i) |> Poly.( = ) Empty then (
      line1.(i) <- Rock;
      line2.(i) <- Empty
    )
  done

let parse_input input =
  input
  |> String.split_lines
  |> Array.of_list
  |> Array.map ~f:(fun line ->
         line |> String.to_list |> Array.of_list |> Array.map ~f:spot_of_char)

let roll_once map =
  for i = 0 to Array.length map - 2 do
    roll_up map.(i) map.(i + 1)
  done

let%test _ =
  let input = {|O...#.
OO..OO|} in
  let map = input |> parse_input in
  let _ = roll_once map in
  map
  |> Poly.( = )
       [|
         [| Rock; Rock; Empty; Empty; Stop; Rock |];
         [| Rock; Empty; Empty; Empty; Rock; Empty |];
       |]

let roll_complete map =
  let finished = ref false in
  while not !finished do
    let before = Array.copy_matrix map in
    let _ = roll_once map in
    if before |> Poly.( = ) map then finished := true
  done;
  map

let string_of_map map =
  let open List.Monad_infix in
  let map = map |> List.of_array >>| List.of_array in
  map
  >>| (fun row -> row >>| char_of_spot |> String.of_list)
  |> String.concat ~sep:"\n"

let want =
  {|OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....|}

let%test _ =
  let map = input |> parse_input |> roll_complete in
  let out = map |> string_of_map in
  out |> String.equal want

let calc_total map =
  let total = ref 0 in
  let start = ref (Array.length map) in
  while !start >= 1 do
    total :=
      !total
      + !start
        * Array.fold
            map.(Array.length map - !start)
            ~init:0
            ~f:(fun acc s ->
              if s |> Poly.( = ) Rock then
                acc + 1
              else
                acc);
    start := !start - 1
  done;
  !total

let%test _ = input |> parse_input |> roll_complete |> calc_total = 136
