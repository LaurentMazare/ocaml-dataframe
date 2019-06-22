open Base
open Dataframe
open Df.R.Let_syntax

let col_i = "col-indexes"
let col_i2 = "col-i2"
let col_sin = "col-sin"
let col_sin_str = "col-sin-str"
let csv_filename = Caml.Filename.temp_file "test" ".csv"

let build_df () =
  let indexes = Array.init 500 ~f:Fn.id in
  let i2 = Array.map indexes ~f:(fun x -> x * x) in
  let sin =
    Array.map indexes ~f:(fun x ->
        Float.of_int x |> Float.sin |> Float.round_decimal ~decimal_digits:3)
  in
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
    ------- ------------ -------- ------------
     col-i2  col-indexes  col-sin  col-sin-str
    ------- ------------ -------- ------------
          0            0       0.           0.
          1            1    0.841        0.841
          4            2    0.909        0.909
          9            3    0.141        0.141
         16            4   -0.757       -0.757
         25            5   -0.959       -0.959
         36            6   -0.279       -0.279
         49            7    0.657        0.657
         64            8    0.989        0.989
         81            9    0.412        0.412 |}]

let%expect_test _ =
  let df = build_df () in
  Csv.write_exn df csv_filename;
  let df =
    Csv.read_exn ~columns:Native_array.[ col_i, pint; col_sin, pfloat ] csv_filename
  in
  let slice_df =
    Df.filter
      df
      [%map_open
        let i = Df.R.int col_i in
        i < 10 || (i <= 50 && i % 10 = 0) || i % 100 = 0]
  in
  Df.print slice_df;
  [%expect
    {|
    ------------ --------
     col-indexes  col-sin
    ------------ --------
               0       0.
               1    0.841
               2    0.909
               3    0.141
               4   -0.757
               5   -0.959
               6   -0.279
               7    0.657
               8    0.989
               9    0.412
              10   -0.544
              20    0.913
              30   -0.988
              40    0.745
              50   -0.262
             100   -0.506
             200   -0.873
             300      -1.
             400   -0.851 |}]
