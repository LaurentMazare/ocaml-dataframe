open Base
open Dataframe
open Df.R.Let_syntax

let col_pi = "pi"
let col_e1 = "e"
let col_e2 = "sum_n 1/n!"

let with_df ~f =
  let pi = Column.of_array Native_array.int [| 3; 1; 4; 1; 5; 9; 2; 6; 5 |] in
  let e = Column.of_array Native_array.float [| 2.; 7.; 1.; 8.; 2.; 8.; 1.; 8.; 2. |] in
  let df = Df.create_exn [ col_e1, P e; col_pi, P pi; col_e2, P e ] in
  f df

let%expect_test _ =
  with_df ~f:(fun df ->
      Df.print df;
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
      Df.print df;
      [%expect
        {|
    --- --- -----------
      e  pi  sum_n 1/n!
    --- --- -----------
     7.   1          7.
     8.   1          8.
    |}]
  )

let%expect_test _ =
  with_df ~f:(fun df ->
      let df = Df.sort_by ~reverse:true df ~name:"e" in
      Df.print df;
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
      Df.print df;
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
      Df.print df;
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
  )

let%expect_test _ =
  with_df ~f:(fun df ->
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
      Df.print df;
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
  )

let%expect_test _ =
  with_df ~f:(fun df ->
      let grouped = Df.group df (Df.R.int col_pi) in
      let grouped = List.sort grouped ~compare:Caml.compare in
      List.iter grouped ~f:(fun (key, df) ->
          Stdio.printf "> %d\n%!" key;
          Df.print df);
      [%expect
        {|
    > 1
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      7.   1          7.
      8.   1          8.
    > 2
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      1.   2          1.
    > 3
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      2.   3          2.
    > 4
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      1.   4          1.
    > 5
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      2.   5          2.
      2.   5          2.
    > 6
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      8.   6          8.
    > 9
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      8.   9          8.
    |}];
      let grouped =
        Df.group
          df
          [%map_open
            let pi = Df.R.int col_pi
            and e = Df.R.float col_e1 in
            (pi + Float.to_int e) % 2]
      in
      let grouped = List.sort grouped ~compare:Caml.compare in
      List.iter grouped ~f:(fun (key, df) ->
          Stdio.printf "> %d\n%!" key;
          Df.print df);
      [%expect
        {|
    > 0
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      7.   1          7.
      8.   6          8.
    > 1
     --- --- -----------
       e  pi  sum_n 1/n!
     --- --- -----------
      2.   3          2.
      1.   4          1.
      8.   1          8.
      2.   5          2.
      8.   9          8.
      1.   2          1.
      2.   5          2.
  |}]
  )

let%expect_test _ =
  with_df ~f:(fun df ->
      let sum_pi =
        [%map_open
          let pi = Df.R.int col_pi in
          fun acc -> acc + pi]
      in
      let sum_pi = Df.fold df ~init:0 ~f:sum_pi in
      Stdio.printf
        "%d %d %f\n%!"
        sum_pi
        (Df.Int.sum df ~name:col_pi)
        (Df.Int.mean df ~name:col_pi |> Option.value ~default:Float.nan);
      [%expect {| 36 36 4.000000 |}];
      let sum_e_nrows =
        [%map_open
          let e = Df.R.float col_e1 in
          fun (acc_sum, acc_cnt) -> acc_sum +. e, acc_cnt + 1]
      in
      let sum_e, nrows = Df.fold df ~init:(0., 0) ~f:sum_e_nrows in
      Stdio.printf
        "%d %f %f\n%!"
        nrows
        sum_e
        (Df.Float.mean df ~name:col_e1 |> Option.value ~default:Float.nan);
      [%expect {| 9 39.000000 4.333333 |}])

let%expect_test _ =
  with_df ~f:(fun df ->
      let df = Df.filter_columns_exn df ~names:[ col_e1; col_e2 ] in
      Df.print df;
      [%expect
        {|
        --- -----------
          e  sum_n 1/n!
        --- -----------
         2.          2.
         7.          7.
         1.          1.
         8.          8.
         2.          2.
         8.          8.
         1.          1.
         8.          8.
         2.          2.
        |}]
  )
