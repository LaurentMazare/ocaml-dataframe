open Base
open Dataframe
open Df.R.Let_syntax

let col_pi = "pi"
let col_e1 = "e"
let col_e2 = "sum_n 1/n!"

let create_df () =
  let pi = Column.of_array Native_array.int [| 3; 1; 4; 1; 5; 9; 2; 6; 5 |] in
  let e = Column.of_array Native_array.float [| 2.; 7.; 1.; 8.; 2.; 8.; 1.; 8.; 2. |] in
  Df.create_exn [ col_e1, P e; col_pi, P pi; col_e2, P e ]

let%expect_test _ =
  let df = create_df () in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
        --- --- -----------
          e  pi  sum_n 1/n!
        --- --- -----------
         2.   3          2.
         7.   1          7.
         1.   4          1.
         8.   1          8.
         2.   5          2.
         8.   9          8.
         1.   2          1.
         8.   6          8.
         2.   5          2.
      |}];
  let df =
    Df.filter
      df
      [%map_open
        let pi = Df.R.int col_pi in
        pi = 1]
  in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
    --- --- -----------
      e  pi  sum_n 1/n!
    --- --- -----------
     7.   1          7.
     8.   1          8.
    |}]

let%expect_test _ =
  let df = create_df () in
  let df = Df.sort_by ~reverse:true df ~name:"e" in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
    --- --- -----------
      e  pi  sum_n 1/n!
    --- --- -----------
     8.   1          8.
     8.   9          8.
     8.   6          8.
     7.   1          7.
     2.   3          2.
     2.   5          2.
     2.   5          2.
     1.   4          1.
     1.   2          1.
  |}];
  let df = Df.sort_by ~reverse:false df ~name:"pi" in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
    --- --- -----------
      e  pi  sum_n 1/n!
    --- --- -----------
     8.   1          8.
     7.   1          7.
     1.   2          1.
     2.   3          2.
     1.   4          1.
     2.   5          2.
     2.   5          2.
     8.   6          8.
     8.   9          8.
    |}];
  let df =
    Df.sort
      df
      ~compare:Caml.compare
      [%map_open
        let pi = Df.R.int col_pi
        and e = Df.R.float col_e1 in
        pi, e]
  in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
    --- --- -----------
      e  pi  sum_n 1/n!
    --- --- -----------
     7.   1          7.
     8.   1          8.
     1.   2          1.
     2.   3          2.
     1.   4          1.
     2.   5          2.
     2.   5          2.
     8.   6          8.
     8.   9          8.
    |}]

let%expect_test _ =
  let df = create_df () in
  let df =
    Df.map_and_add_column_exn
      df
      ~name:"e^2 + pi"
      Native_array.float
      [%map_open
        let pi = Df.R.int col_pi
        and e = Df.R.float col_e1 in
        (e *. e) +. Float.of_int pi]
  in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  [%expect
    {|
    --- --------- --- -----------
      e  e^2 + pi  pi  sum_n 1/n!
    --- --------- --- -----------
     2.        7.   3          2.
     7.       50.   1          7.
     1.        5.   4          1.
     8.       65.   1          8.
     2.        9.   5          2.
     8.       73.   9          8.
     1.        3.   2          1.
     8.       70.   6          8.
     2.        9.   5          2.
    |}]
