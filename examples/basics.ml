open! Base
open! Dataframe

let () =
  let pi = Column.of_array Native_array.int [| 3; 1; 4; 1; 5; 9; 2; 6; 5 |] in
  let e = Column.of_array Native_array.float [| 2.; 7.; 1.; 8.; 2.; 8.; 1.; 8.; 2. |] in
  let df = Df.create_exn [ "pi", P pi; "e", P e ] in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline;
  let df2 =
    Df.filter df Df.Filter.(float df "e" @-> int df "pi" @-> return) (fun _e pi -> pi = 1)
  in
  List.iter (Df.to_aligned_rows df2) ~f:Stdio.print_endline;
  let df = Df.App.(filter (apply (return (fun pi -> pi = 1)) (int "pi")) df) in
  List.iter (Df.to_aligned_rows df) ~f:Stdio.print_endline
