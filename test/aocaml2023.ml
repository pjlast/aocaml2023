open OUnit2
open Day11

let tests =
  "test suite for transpose"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (transpose []));
         ( "single element" >:: fun _ ->
           assert_equal [ [ 1 ] ] (transpose [ [ 1 ] ])
             ~printer:
               (Stdlib.Format.asprintf "%a" (Fmt.list (Fmt.list Fmt.int))) );
         ( "rows become columns" >:: fun _ ->
           assert_equal [ [ 1 ]; [ 2 ] ]
             (transpose [ [ 1; 2 ] ])
             ~printer:
               (Stdlib.Format.asprintf "%a" (Fmt.list (Fmt.list Fmt.int))) );
         ( "columns become rows" >:: fun _ ->
           assert_equal
             [ [ 1; 2 ] ]
             (transpose [ [ 1 ]; [ 2 ] ])
             ~printer:
               (Stdlib.Format.asprintf "%a" (Fmt.list (Fmt.list Fmt.int))) );
         ( "transpose matrix" >:: fun _ ->
           assert_equal
             [ [ 1; 2 ]; [ 3; 4 ] ]
             (transpose [ [ 1; 3 ]; [ 2; 4 ] ])
             ~printer:
               (Stdlib.Format.asprintf "%a" (Fmt.list (Fmt.list Fmt.int))) );
       ]

let _ = run_test_tt_main tests
