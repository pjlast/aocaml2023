open Base

let input = [ (7, 9); (15, 40); (30, 200) ]

let calc_ways_to_win time dist =
  let secs = List.range 0 (time + 1) in
  List.filter_map secs ~f:(fun t ->
      let d = (time - t) * t in
      if d > dist then Some t else None)

let%test _ = calc_ways_to_win 7 9 |> List.length = 4
let%test _ = calc_ways_to_win 15 40 |> List.length = 8

let process_races races =
  List.fold
    (List.map races ~f:(fun (time, dist) ->
         calc_ways_to_win time dist |> List.length))
    ~init:1 ~f:( * )

let%test _ = process_races input = 288
let input_2 = [ (71530, 940200) ]

let calc_input races =
  List.fold races ~init:("", "") ~f:(fun (time', distance') (time, distance) ->
      (time' ^ (time |> Int.to_string), distance' ^ (distance |> Int.to_string)))
  |> fun (time, distance) -> (time |> Int.of_string, distance |> Int.of_string)

let%test _ = process_races [ calc_input input ] = 71503
