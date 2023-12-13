let () =
  let x = [| [| 0 |]; [| 2 |]; [| 4 |]; [| 6 |] |] in
  x.(0).(0) <- 8;
  print_endline (string_of_int x.(0).(0))
