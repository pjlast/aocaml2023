(** [first_num_exn s] returns the first digit found in [s] *)
val first_num_exn : string -> int

(** [last_num_exn s] returns the last digit found in [s] *)
val last_num_exn : string -> int

(** [first_num_2_exn s] returns the first number found in [s], which could be
    either a digit, or a word like "one" *)
val first_num_2_exn : string -> int list -> int list
