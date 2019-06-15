open Base

let read filename =
  let csv = Csv.load filename in
  if Csv.columns csv = 0
  then Or_error.errorf "no columns in csv file %s" filename
  else (
    let lines = Csv.lines csv in
    if lines = 0
    then Or_error.errorf "no rows in csv file %s" filename
    else (
      let csv = Csv.transpose csv in
      List.map csv ~f:(function
          | [] -> assert false (* should be caught by earlier checks *)
          | header :: data ->
            let column = Column.of_array Native_array.string (List.to_array data) in
            header, Column.P column)
      |> Df.create))

let read_exn filename = Or_error.ok_exn (read filename)

let write_exn (type a) (t : a Df.t) filename =
  Df.named_columns t
  |> List.map ~f:(fun (header, column) ->
         let column =
           match Df.filter_ t with
           | No_filter len ->
             List.init len ~f:(fun index -> Column.packed_get_string column index)
           | Filter filter ->
             Bool_array.indexes filter ~value:true
             |> Array.map ~f:(Column.packed_get_string column)
             |> Array.to_list
         in
         header :: column)
  |> Csv.transpose
  |> Csv.save filename

let write t filename = Or_error.try_with (fun () -> write_exn t filename)
