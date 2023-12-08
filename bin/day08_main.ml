open Base

type node = {
  code : string;
  mutable left : node option;
  mutable right : node option;
}

let tbl : (string, node) Hashtbl.t = Hashtbl.create (module String)

let parse_loc line =
  match
    line
    |> String.strip ~drop:(fun c ->
           if c |> Char.equal '(' || c |> Char.equal ')' || c |> Char.equal ' '
           then
             true
           else
             false)
    |> String.split ~on:','
  with
  | [ left; right ] -> (String.strip left, String.strip right)
  | _ -> failwith "invalid loc"

let parse_line line =
  match line |> String.split ~on:'=' |> List.map ~f:String.strip with
  | [ pos; loc ] -> (pos, parse_loc loc)
  | _ -> failwith "invalid line"

type direction = L | R

let char_to_direction = function
  | 'L' -> L
  | 'R' -> R
  | _ -> failwith "invalid direction"

let _dir_to_str = function
  | L -> "L"
  | R -> "R"

let steps =
  String.to_list
    {|LLRLRRRLRRRLRRLLRRRLLRRLLRLRLRRRLRRRLLRRRLLRRRLRRLRRLRLRRLLRRRLRRRLLRRRLRRLLLRRLRLLLRLRRRLRLRLLLRRLRRLLLRRRLLRRRLRLRLLRRLRLRRRLRLRLLRLRRLRRRLRRLRLRRRLRLRRLRRLRLRRLLRLRLRRLRLLRRLRRLRLRRLLRLRLLRRLLRLLLRRLRLRRRLRRRLRRRLRLRLRRRLLLRLRRLRLRRRLRRRLRRRLRLRRRLRRRLRRRLRRRR|}
  |> List.map ~f:char_to_direction

let rec take_step steps' =
  match steps' with
  | h :: t -> (h, t)
  | [] -> take_step steps

let travel { left; right; _ } step =
  match step with
  | L -> (
      match left with
      | Some x -> x
      | None -> failwith "NONE")
  | R -> (
      match right with
      | Some x -> x
      | None -> failwith "NONE")

let part_1 () =
  match Stdio.In_channel.read_all "inputs/day08.txt" |> String.split_lines with
  | _movements :: _ :: map ->
      let rec aux l =
        match l with
        | h :: t ->
            let _ =
              parse_line h |> fun (pos, (left, right)) ->
              let curr =
                match Hashtbl.find tbl pos with
                | Some curr -> curr
                | None ->
                    let node = { code = pos; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:pos ~data:node in
                    node
              in
              let left' =
                match Hashtbl.find tbl left with
                | Some x -> x
                | None ->
                    let node = { code = left; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:left ~data:node in
                    node
              in
              let right' =
                match Hashtbl.find tbl right with
                | Some x -> x
                | None ->
                    let node = { code = right; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:right ~data:node in
                    node
              in
              curr.left <- Some left';
              curr.right <- Some right'
            in
            aux t
        | _ -> ()
      in
      aux map;
      Stdio.print_endline "Loaded up";

      let first = Hashtbl.find_exn tbl "AAA" in

      let rec solve count pos steps =
        let { code; _ } = pos in
        match code with
        | "ZZZ" -> count
        | _ ->
            let step, rest = take_step steps in
            let next = travel pos step in
            solve (count + 1) next rest
      in
      solve 0 first steps
  | _ -> failwith "Invalid input"

let part_2 () =
  match Stdio.In_channel.read_all "inputs/day08.txt" |> String.split_lines with
  | _movements :: _ :: map ->
      let rec aux l =
        match l with
        | h :: t ->
            let _ =
              parse_line h |> fun (pos, (left, right)) ->
              let curr =
                match Hashtbl.find tbl pos with
                | Some curr -> curr
                | None ->
                    let node = { code = pos; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:pos ~data:node in
                    node
              in
              let left' =
                match Hashtbl.find tbl left with
                | Some x -> x
                | None ->
                    let node = { code = left; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:left ~data:node in
                    node
              in
              let right' =
                match Hashtbl.find tbl right with
                | Some x -> x
                | None ->
                    let node = { code = right; left = None; right = None } in
                    let _ = Hashtbl.add tbl ~key:right ~data:node in
                    node
              in
              curr.left <- Some left';
              curr.right <- Some right'
            in
            aux t
        | _ -> ()
      in
      aux map;
      Stdio.print_endline "Loaded up";

      let inputs =
        Hashtbl.keys tbl
        |> List.filter ~f:(fun k ->
               match String.to_list k with
               | [ _; _; 'A' ] -> true
               | _ -> false)
        |> List.map ~f:(Hashtbl.find_exn tbl)
      in

      let rec solve count pos steps =
        let { code; _ } = pos in
        match String.to_list code with
        | [ _; _; 'Z' ] -> count
        | _ ->
            let step, rest = take_step steps in
            let next = travel pos step in
            solve (count + 1) next rest
      in
      inputs |> List.map ~f:(fun i -> solve 0 i steps)
  | _ -> failwith "Invalid input"

let gcd a b =
  let rec aux i =
    if i > a || i > b then
      1
    else if a % i = 0 && b % i = 0 then
      i
    else
      aux (i + 1)
  in
  aux 2

let lcm a b = a * b / gcd a b

let () =
  Stdio.print_endline "day 8 part 1";
  Stdio.print_endline (Int.to_string (part_1 ()));

  Stdio.print_endline "day 8 part 2";
  part_2 ()
  |> List.reduce ~f:( lcm )
  |> Option.value_exn
  |> Int.to_string
  |> Stdio.print_endline
