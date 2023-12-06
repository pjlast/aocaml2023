open Day06
open Stdio

let input = [ (54, 302); (94, 1476); (65, 1029); (92, 1404) ]
let input2 = [ (54946592, 302147610291404) ]

let () =
    print_endline "day 6 part 1";
    print_endline (Int.to_string (process_races input));

    print_endline "day 6 part 2";
    print_endline (Int.to_string (process_races input2))
