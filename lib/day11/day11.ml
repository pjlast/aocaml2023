open Base

let input =
  {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|}

let want_output =
  {|....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......|}

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map xss ~f:List.hd_exn)
      :: transpose (xs :: List.map xss ~f:List.tl_exn)

let%test _ =
  transpose [ [ 1; 2 ]; [ 3; 4 ] ] |> Poly.( = ) [ [ 1; 3 ]; [ 2; 4 ] ]

let char_list_of_string str =
  str |> String.split_lines |> List.map ~f:String.to_list

let contains_galaxy row =
  match List.find row ~f:(fun e -> e |> Char.equal '#') with
  | None -> false
  | Some _ -> true

let contains_galaxy_2 row =
  match List.find row ~f:(fun (_, e) -> e |> Char.equal '#') with
  | None -> false
  | Some _ -> true

let expand_rows map =
  let rec aux map acc =
    match map with
    | [] -> acc
    | h :: t ->
        if contains_galaxy h then
          aux t (acc @ [ h ])
        else
          aux t (acc @ [ h; h ])
  in
  aux map []

let rec repeat el n acc =
  if n > 0 then
    repeat el (n - 1) ([ el ] @ acc)
  else
    acc

let%test _ =
  let inl = {|...|} |> String.to_list in
  let repeated = repeat inl 2 [] in
  repeated |> Poly.( = ) [ [ '.'; '.'; '.' ]; [ '.'; '.'; '.' ] ]

let empty =
  let inl =
    {|............................................................................................................................................|}
    |> String.to_list
  in
  repeat inl 1000000 []

let empty_hundred =
  let inl = {|..........|} |> String.to_list in
  repeat inl 2 []

let expand_rows_million map =
  let rec aux map acc =
    match map with
    | [] -> acc
    | h :: t ->
        if contains_galaxy h then
          aux t (acc @ [ h ])
        else
          aux t (acc @ repeat h 1000000 [])
  in
  aux map []

let expand_rows_hundred map =
  let rec aux map acc count =
    match map with
    | [] -> acc
    | h :: t ->
        if contains_galaxy h then
          aux t (acc @ [ h |> List.map ~f:(fun a -> (count, a)) ]) (count + 1)
        else
          aux t
            (acc @ [ h |> List.map ~f:(fun a -> (count, a)) ])
            (count + 1000000)
  in
  aux map [] 0

let expand_rows_hundred_2 map =
  let rec aux map acc count =
    match map with
    | [] -> acc
    | h :: t ->
        if contains_galaxy_2 h then
          aux t (acc @ [ h |> List.map ~f:(fun a -> (count, a)) ]) (count + 1)
        else
          aux t
            (acc @ [ h |> List.map ~f:(fun a -> (count, a)) ])
            (count + 1000000)
  in
  aux map [] 0

let map_to_string map =
  String.concat_lines (List.map map ~f:String.of_char_list) |> String.strip

let%test _ =
  let output =
    input
    |> char_list_of_string
    |> expand_rows
    |> transpose
    |> expand_rows
    |> transpose
    |> map_to_string
  in
  output |> String.equal want_output

let expand_galaxy map =
  map |> expand_rows |> transpose |> expand_rows |> transpose

let expand_galaxy_million map =
  let transposed = map |> expand_rows_hundred |> transpose in
  transposed |> expand_rows_hundred_2 |> transpose

let get_galaxies map =
  map
  |> List.filter_mapi ~f:(fun i row ->
         let j =
           List.filter_mapi row ~f:(fun j c ->
               if c |> Char.equal '#' then
                 Some j
               else
                 None)
         in
         match j with
         | [] -> None
         | j -> Some (j |> List.map ~f:(fun j -> (i, j))))
  |> Stdlib.List.flatten

let get_galaxies_2 map =
  map
  |> List.filter_mapi ~f:(fun _ row ->
         let j =
           List.filter_mapi row ~f:(fun _ (a, (b, c)) ->
               if c |> Char.equal '#' then
                 Some (a, b)
               else
                 None)
         in
         match j with
         | [] -> None
         | j -> Some (j |> List.map ~f:(fun j -> j)))
  |> Stdlib.List.flatten

let%test _ =
  let input_map = {|#..#
.##.|} in
  let map = input_map |> char_list_of_string in
  let out = map |> get_galaxies in
  out |> Poly.( = ) [ (0, 0); (0, 3); (1, 1); (1, 2) ]

let dist (y1, x1) (y2, x2) = Int.abs (y1 - y2) + Int.abs (x1 - x2)

let map_pairs list ~f =
  let rec aux list acc =
    match list with
    | [] -> acc
    | _ :: [] -> acc
    | h :: t -> aux t (acc @ List.map t ~f:(f h))
  in
  aux list []

let%test _ =
  let input_map = {|#..#
.##.|} in
  let map = input_map |> char_list_of_string in
  let galaxies = map |> get_galaxies in
  let dists = map_pairs galaxies ~f:(fun a b -> dist a b) in
  dists |> Poly.( = ) [ 3; 2; 3; 3; 2; 1 ]

let%test _ =
  let map = input |> char_list_of_string |> expand_galaxy in
  let galaxies = map |> get_galaxies in
  let dists = map_pairs galaxies ~f:dist in
  let sum = List.fold dists ~init:0 ~f:( + ) in
  sum = 374
