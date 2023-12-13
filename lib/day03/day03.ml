open Core

type coord = int * int

(** Extract all symbols from a string into a list of (row, col) coordinates *)
let symbol_list_of_string str =
  List.fold
    (List.mapi (String.split_lines str) ~f:(fun i line ->
         List.filter_mapi (String.to_list line) ~f:(fun j c ->
             match c with
             | c when Char.is_digit c || Char.equal '.' c -> None
             | _ -> Some (i, j))))
    ~init:[] ~f:( @ )

let%test _ =
  let input = {|467..114..
...*......
..35..633.|} in
  let want = [ (1, 3) ] in
  let got = symbol_list_of_string input in
  List.equal (fun (a, b) (c, d) -> a = c && b = d) want got

type number = { start : coord; end' : coord; value : int }

(** Extract a list of numbers from a single string line (which should be a
    [char list]) *)
let rec number_list_of_string_line str_l row curr_col (curr_num : number) acc =
  match str_l with
  | [] ->
      if curr_num.value = 0 then
        acc
      else
        acc
        @ [
            {
              start = curr_num.start;
              end' = (row, curr_col - 1);
              value = curr_num.value;
            };
          ]
  | hd :: tl when not (Char.is_digit hd) ->
      if curr_num.value > 0 then
        number_list_of_string_line tl row (curr_col + 1)
          { start = (0, 0); end' = (0, 0); value = 0 }
          (acc
          @ [
              {
                start = curr_num.start;
                end' = (row, curr_col - 1);
                value = curr_num.value;
              };
            ])
      else
        number_list_of_string_line tl row (curr_col + 1)
          { start = (0, 0); end' = (0, 0); value = 0 }
          acc
  | hd :: tl ->
      if curr_num.value = 0 then
        number_list_of_string_line tl row (curr_col + 1)
          {
            start = (row, curr_col);
            end' = curr_num.end';
            value = hd |> String.of_char |> Int.of_string;
          }
          acc
      else
        number_list_of_string_line tl row (curr_col + 1)
          {
            start = curr_num.start;
            end' = curr_num.end';
            value =
              (curr_num.value * 10) + (hd |> String.of_char |> Int.of_string);
          }
          acc

let int_int_equal (a, b) (c, d) = a = c && b = d

let%test _ =
  let input = {|467..114|} in
  let want =
    [
      { start = (1, 0); end' = (1, 2); value = 467 };
      { start = (1, 5); end' = (1, 7); value = 114 };
    ]
  in
  let got =
    number_list_of_string_line (String.to_list input) 1 0
      { start = (0, 0); end' = (0, 0); value = 0 }
      []
  in
  List.equal
    (fun a b ->
      int_int_equal a.start b.start
      && int_int_equal a.end' b.end'
      && a.value = b.value)
    want got

(** Extracts a list of numbers from an input string *)
let number_list_of_string str =
  List.foldi (String.split_lines str) ~init:[] ~f:(fun i acc x ->
      acc
      @ number_list_of_string_line (String.to_list x) i 0
          { start = (0, 0); end' = (0, 0); value = 0 }
          [])

let%test _ =
  let input = {|467..114..
...*......
..35..633.|} in
  let want =
    [
      { start = (0, 0); end' = (0, 2); value = 467 };
      { start = (0, 5); end' = (0, 7); value = 114 };
      { start = (2, 2); end' = (2, 3); value = 35 };
      { start = (2, 6); end' = (2, 8); value = 633 };
    ]
  in
  let got = number_list_of_string input in
  List.equal
    (fun a b ->
      int_int_equal a.start b.start
      && int_int_equal a.end' b.end'
      && a.value = b.value)
    want got

(** Compare a number with a given (row, col) coordinate and return true if the
    number is adjacent to the coordinate in any direction, including diagonally *)
let num_and_coord_is_neighbour { start = numrow, startcol; end' = _, endcol; _ }
    (row, col) =
  (row >= numrow - 1 && row <= numrow + 1)
  && col >= startcol - 1
  && col <= endcol + 1

let%test _ =
  num_and_coord_is_neighbour { start = (1, 0); end' = (1, 2); value = 2 } (2, 3)

let%test _ =
  num_and_coord_is_neighbour { start = (3, 4); end' = (3, 8); value = 2 } (2, 3)

let%test _ =
  num_and_coord_is_neighbour { start = (3, 4); end' = (3, 8); value = 2 } (4, 9)

let sum_valid_nums nummap symmap =
  let vals_with_neighbours =
    List.filter_map nummap ~f:(fun num ->
        if
          Option.is_some
            (List.find symmap ~f:(fun sym -> num_and_coord_is_neighbour num sym))
        then
          Some num.value
        else
          None)
  in
  List.fold vals_with_neighbours ~init:0 ~f:(fun acc x -> acc + x)

let%test _ =
  let input =
    {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
  in
  let symmap = symbol_list_of_string input in
  let nummap = number_list_of_string input in
  sum_valid_nums nummap symmap = 4361

(** Extracts a list of coordinates for all '*' symbols in a string *)
let gear_list_of_string str =
  List.fold
    (List.mapi (String.split_lines str) ~f:(fun i line ->
         List.filter_mapi (String.to_list line) ~f:(fun j c ->
             match c with
             | '*' -> Some (i, j)
             | _ -> None)))
    ~init:[]
    ~f:(fun acc x -> acc @ x)

let%test _ =
  let input = {|467..114..
...*...$..
..35..633.|} in
  let want = [ (1, 3) ] in
  let got = gear_list_of_string input in
  List.equal (fun (a, b) (c, d) -> a = c && b = d) want got

(** Given a list of coordinates and a list of numbers, returns the value of each
    number adjacent to a gear, if and only if that gear has exactly two adjacent
    numbers *)
let nums_adjacent_gears gears nums =
  List.filter_map gears ~f:(fun gear ->
      let adjacent_nums =
        List.filter_map nums ~f:(fun num ->
            if num_and_coord_is_neighbour num gear then
              Some num.value
            else
              None)
      in
      if List.length adjacent_nums = 2 then
        Some adjacent_nums
      else
        None)

let%test _ =
  let input =
    {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
  in
  let gearmap = gear_list_of_string input in
  let nummap = number_list_of_string input in
  let valid_vals = nums_adjacent_gears gearmap nummap in
  let sum =
    List.fold
      (List.fold valid_vals ~init:[] ~f:(fun acc valid_val ->
           acc @ [ List.fold valid_val ~init:1 ~f:( * ) ]))
      ~init:0 ~f:( + )
  in
  sum = 467835
