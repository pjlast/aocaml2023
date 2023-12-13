type coord
type number

(** Extract all symbols from a string into a list of (row, col) coordinates *)
val symbol_list_of_string : string -> coord list

(** Extracts a list of numbers from an input string *)
val number_list_of_string : string -> number list

(** Returns the sum of the [value]s of all numbers where a number is adjacent to
    at least one [coord] *)
val sum_valid_nums : number list -> coord list -> int

(** Extract the coordinates of all '*' symbols in a string. *)
val gear_list_of_string : string -> coord list

(** Given a list of coordinates and a list of numbers, returns the value of each
    number adjacent to a gear, if and only if that gear has exactly two adjacent
    numbers *)
val nums_adjacent_gears : coord list -> number list -> int list list
