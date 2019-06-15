open Base
open Dataframe
open! Df.R.Let_syntax

let col_i = "col-indexes"
let col_i2 = "col-i2"
let col_sin = "col-sin"
let col_sin_str = "col-sin-str"
let csv_filename = "test.csv"

let build_df () =
  let indexes = Array.init 500 ~f:Fn.id in
  let i2 = Array.map indexes ~f:(fun x -> x * x) in
  let sin = Array.map indexes ~f:(fun x -> Float.of_int x |> Float.sin) in
  let sin_str = Array.map sin ~f:Float.to_string in
  let columns =
    [ (col_i, Column.(P (of_array Native_array.int indexes)))
    ; (col_i2, Column.(P (of_array Native_array.int i2)))
    ; (col_sin, Column.(P (of_array Native_array.float sin)))
    ; (col_sin_str, Column.(P (of_array Native_array.string sin_str)))
    ]
  in
  Df.create_exn columns

let%expect_test _ =
  let df = build_df () in
  Csv.write_exn df csv_filename;
  let df = Csv.read_exn csv_filename in
  let slice_df =
    Df.filter
      df
      [%map_open
        let i = Df.R.string col_i in
        let i = Int.of_string i in
        i < 10]
  in
  Df.print slice_df;
  [%expect
    {|
    ------- ------------ --------------------- ---------------------
     col-i2  col-indexes               col-sin           col-sin-str
    ------- ------------ --------------------- ---------------------
          0            0                    0.                    0.
          1            1    0.8414709848078965    0.8414709848078965
          4            2   0.90929742682568171   0.90929742682568171
          9            3   0.14112000805986721   0.14112000805986721
         16            4   -0.7568024953079282   -0.7568024953079282
         25            5  -0.95892427466313845  -0.95892427466313845
         36            6  -0.27941549819892586  -0.27941549819892586
         49            7   0.65698659871878906   0.65698659871878906
         64            8   0.98935824662338179   0.98935824662338179
         81            9   0.41211848524175659   0.41211848524175659 |}]

let%expect_test _ =
  let df = build_df () in
  Csv.write_exn df csv_filename;
  let df =
    Csv.read_exn
      ~columns:
        [ col_i, Array_intf.P Native_array.int; col_sin, Array_intf.P Native_array.float ]
      csv_filename
  in
  let slice_df =
    Df.filter
      df
      [%map_open
        let i = Df.R.int col_i in
        i < 10]
  in
  Df.print slice_df;
  [%expect
    {|
    ------------ ---------------------
     col-indexes               col-sin
    ------------ ---------------------
               0                    0.
               1    0.8414709848078965
               2   0.90929742682568171
               3   0.14112000805986721
               4   -0.7568024953079282
               5  -0.95892427466313845
               6  -0.27941549819892586
               7   0.65698659871878906
               8   0.98935824662338179
               9   0.41211848524175659 |}]
