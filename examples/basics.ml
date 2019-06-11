open! Base
open! Dataframe
open Df.R.Let_syntax

let col_pi = "pi"
let col_e1 = "e"
let col_e2 = "sum_n 1/n!"

let () =
  let pi = Column.of_array Native_array.int [| 3; 1; 4; 1; 5; 9; 2; 6; 5 |] in
  let e = Column.of_array Native_array.float [| 2.; 7.; 1.; 8.; 2.; 8.; 1.; 8.; 2. |] in
  let df = Df.create_exn [ col_e1, P e; col_pi, P pi; col_e2, P e ] in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  let filtered_df =
    Df.filter
      df
      [%map_open
        let pi = Df.R.int col_pi in
        pi = 1]
  in
  List.iter (Df.to_aligned_rows filtered_df) ~f:Stdio.print_endline;
  let sum_df =
    Df.map
      filtered_df
      Native_array.float
      [%map_open
        let pi = Df.R.int col_pi
        and e = Df.R.float col_e2 in
        Float.of_int pi +. e]
  in
  Stdio.printf "> %d\n%!" (Df.length df);
  Stdio.print_endline (Column.to_string sum_df);
  let sum_df =
    Df.map
      df
      Native_array.float
      Df.R.(map2 (int col_pi) (float col_e2) ~f:(fun pi e -> Float.of_int pi +. e))
  in
  Stdio.printf "> %d\n%!" (Df.length df);
  Stdio.print_endline (Column.to_string sum_df);
  let sorted_df = Df.sort df (Df.R.int col_pi) ~compare:Int.compare in
  List.iter (Df.to_aligned_rows sorted_df) ~f:Stdio.print_endline
