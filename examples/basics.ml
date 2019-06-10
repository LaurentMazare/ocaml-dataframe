open! Base
open! Dataframe
open Df.Row_map.Let_syntax

let col_pi = "pi"
let col_e1 = "e"
let col_e2 = "sum_n 1/n!"

let () =
  let pi = Column.of_array Native_array.int [| 3; 1; 4; 1; 5; 9; 2; 6; 5 |] in
  let e = Column.of_array Native_array.float [| 2.; 7.; 1.; 8.; 2.; 8.; 1.; 8.; 2. |] in
  let df = Df.create_exn [ col_e1, P e; col_pi, P pi; col_e2, P e ] in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  let df =
    Df.filter
      df
      [%map_open
        let pi = Df.Row_map.int "pi" in
        pi = 1]
  in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline
