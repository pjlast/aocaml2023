open Base

let red = 12
let green = 13
let blue = 14

type t = { id : int; hands : Hand.t list }

let _split_id_and_games line =
  let get_id_part l =
    match String.split l ~on:':' with
    | [ id; games ] -> (id, games)
    | _ -> failwith "Not a valid line"
  in

  let id, games = get_id_part line in

  ( (match String.to_list id with
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: t ->
        Int.of_string (String.of_char_list t)
    | _ -> failwith "Not a valid line"),
    games )

let score game =
  let found =
    List.find game.hands ~f:(fun hand ->
        hand.red > red || hand.green > green || hand.blue > blue)
  in

  match found with
  | Some _ -> 0
  | None -> game.id

let power game =
  let red =
    Option.value
      (List.max_elt
         (List.map game.hands ~f:(fun hand -> hand.red))
         ~compare:(fun a b -> Int.compare a b))
      ~default:0
  in
  let green =
    Option.value
      (List.max_elt
         (List.map game.hands ~f:(fun hand -> hand.green))
         ~compare:(fun a b -> Int.compare a b))
      ~default:0
  in
  let blue =
    Option.value
      (List.max_elt
         (List.map game.hands ~f:(fun hand -> hand.blue))
         ~compare:(fun a b -> Int.compare a b))
      ~default:0
  in

  red * green * blue

let of_string_exn line =
  let id, games = _split_id_and_games line in
  let games = String.strip games |> String.split ~on:';' in
  let hands = List.map games ~f:Hand.of_string in
  { id; hands }
