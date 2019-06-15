open Base

let read ?columns filename =
  let open Or_error.Let_syntax in
  let%bind csv = Or_error.try_with (fun () -> Csv.load filename) in
  let%bind () =
    if Csv.columns csv <> 0
    then Ok ()
    else Or_error.errorf "no columns in csv file %s" filename
  in
  let lines = Csv.lines csv in
  let%bind () =
    if lines > 0 then Ok () else Or_error.errorf "no rows in csv file %s" filename
  in
  let csv = Csv.transpose csv in
  let%bind columns =
    match columns with
    | None ->
      let columns =
        List.map csv ~f:(function
            | [] -> Or_error.errorf "no data found in %s" filename
            | header :: data ->
              let column = Column.of_array Native_array.string (List.to_array data) in
              Ok (header, Column.P column))
      in
      Ok columns
    | Some columns ->
      let%bind () =
        let column_names = List.map columns ~f:fst in
        match List.find_all_dups column_names ~compare:String.compare with
        | [] -> Ok ()
        | dups -> Or_error.errorf "duplicate columns %s" (String.concat dups ~sep:",")
      in
      let csv_columns =
        List.map csv ~f:(function
            | [] -> Or_error.errorf "no data found in %s" filename
            | header :: data -> Ok (header, data))
      in
      let%bind csv_columns = Or_error.all csv_columns in
      let%bind csv_columns =
        match Map.of_alist (module String) csv_columns with
        | `Ok map -> Ok map
        | `Duplicate_key key -> Or_error.errorf "duplicate header %s in %s" key filename
      in
      let columns =
        List.map columns ~f:(fun (name, Array_intf.P a) ->
            match Map.find csv_columns name with
            | None -> Or_error.errorf "cannot find column %s in %s" name filename
            | Some data ->
              Or_error.try_with (fun () ->
                  let (module M) = a in
                  let data =
                    List.to_array data
                    |> Array.map ~f:(fun elt ->
                           match M.Elt.of_string elt with
                           | None ->
                             Printf.failwithf
                               "cannot parse %s in column %s when reading %s"
                               elt
                               name
                               filename
                               ()
                           | Some v -> v)
                  in
                  name, Column.P (Column.of_array a data)))
      in
      Ok columns
  in
  let%bind columns = Or_error.all columns in
  Df.create columns

let read_exn ?columns filename = Or_error.ok_exn (read ?columns filename)

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
