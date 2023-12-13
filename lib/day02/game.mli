type t

(** [score game] returns the id of [game] if it is a valid game, otherwise [0].
    A game is valid if it has maximum values for red, green, and blue of 12, 13,
    14 respectively. *)
val score : t -> int

(** [of_string_exn s] returns the game represented by [s]. *)
val of_string_exn : string -> t

(** [power game] returns [game.red * game.green * game.blue]. *)
val power : t -> int
