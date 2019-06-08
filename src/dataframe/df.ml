open Base

(* TODO: use a compact version of this. *)
module Bool_array : sig
  type t

  val create : len:int -> bool -> t
end = struct
  type t = bool array

  let create = Array.create
end

type t =
  { columns : (string, Column.packed, String.comparator_witness) Map.t
  ; filter : Bool_array.t
  }

let create named_columns =
  match named_columns with
  | [] -> Or_error.error_string "no column"
  | (first_column_name, first_column) :: _ ->
    let first_column_length = Column.packed_length first_column in
    let columns_with_different_length =
      List.filter_map named_columns ~f:(fun (name, column) ->
          let column_length = Column.packed_length column in
          if column_length <> first_column_length
          then Some (name, column_length)
          else None)
    in
    if not (List.is_empty columns_with_different_length)
    then
      Or_error.errorf
        "length mismatch, %s: %d, %s"
        first_column_name
        first_column_length
        (List.map columns_with_different_length ~f:(fun (name, length) ->
             Printf.sprintf "%s: %d" name length)
        |> String.concat ~sep:", ")
    else (
      match Map.of_alist (module String) named_columns with
      | `Ok columns ->
        let filter = Bool_array.create ~len:first_column_length true in
        Ok { columns; filter }
      | `Duplicate_key column_name ->
        Or_error.errorf "duplicate column name %s" column_name)

let create_exn columns = create columns |> Or_error.ok_exn
let read_csv _filename = failwith "TODO"
let get_column t column_name = Map.find t.columns column_name
let get_column_exn t column_name = Option.value_exn (get_column t column_name)
let column_names t = Map.keys t.columns
let named_columns t = Map.to_alist t.columns

let to_string ?(header_only = false) t =
  let named_columns = named_columns t in
  let header =
    List.map named_columns ~f:(fun (name, column) ->
        name ^ ": " ^ Column.packed_elt_name column)
    |> String.concat ~sep:"\n"
  in
  let header = header ^ "\n---\n" in
  if header_only then header else failwith "TODO"
