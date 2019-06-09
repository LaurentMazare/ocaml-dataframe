open Base

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

let to_string ?(headers_only = false) t =
  let named_columns = named_columns t in
  let header =
    List.map named_columns ~f:(fun (name, column) ->
        name ^ ": " ^ Column.packed_elt_name column)
    |> String.concat ~sep:"\n"
  in
  if headers_only
  then header
  else (
    let values =
      List.map named_columns ~f:(fun (name, column) ->
          (* TODO: handle filters. *)
          name ^ ":\n[\n" ^ Column.packed_to_string column ^ "]")
      |> String.concat ~sep:"\n"
    in
    header ^ "\n---\n" ^ values)

let length t = Bool_array.num_set t.filter
let num_rows = length
let num_cols t = Map.length t.columns

let to_aligned_rows t =
  let named_columns = named_columns t |> Array.of_list in
  let max_len_per_column =
    Array.map named_columns ~f:(fun (name, _) -> String.length name)
  in
  let escape =
    String.Escaping.escape_gen_exn
      ~escapeworthy_map:[ '\n', 'n'; '\t', 't' ]
      ~escape_char:'\\'
    |> Staged.unstage
  in
  let rows =
    List.init (Bool_array.length t.filter) ~f:(fun j ->
        if Bool_array.get t.filter j
        then
          Array.mapi named_columns ~f:(fun i (_, column) ->
              let str = Column.packed_get_string column j |> escape in
              max_len_per_column.(i) <- max max_len_per_column.(i) (String.length str);
              str)
          |> Option.some
        else None)
    |> List.filter_opt
  in
  let header = Array.map named_columns ~f:fst in
  let delim = Array.map max_len_per_column ~f:(fun l -> String.make (l + 1) '-') in
  List.map (header :: delim :: rows) ~f:(fun row ->
      Array.mapi row ~f:(fun i cell ->
          let col_len = 2 + max_len_per_column.(i) in
          let pad = col_len - String.length cell in
          String.make pad ' ' ^ cell)
      |> String.concat_array)

let copy t =
  { columns = Map.map t.columns ~f:Column.(packed_copy ~filter:t.filter)
  ; filter = Bool_array.create true ~len:(Bool_array.num_set t.filter)
  }

(* Applicative module for filtering. *)
module Filter = struct
  type 'a column = P : ('a, 'b) Column.t -> 'a column

  let column : type a b.
      t -> string -> (module Array_intf.S with type Elt.t = a and type t = b) -> a column
    =
   fun df name mod_ -> P (Column.extract_exn (get_column_exn df name) mod_)

  let get : type a. a column -> int -> a =
   fun (P column) index -> Column.get column index

  type _ t =
    | Apply : 'a column * 'b t -> ('a -> 'b) t
    | Return : bool t

  let return = Return
  let apply w t = Apply (w, t)
  let ( @-> ) = apply
  let int df name = column df name Native_array.int
  let float df name = column df name Native_array.float
  let string df name = column df name Native_array.string
end

let filter t filter_ fn =
  let filter =
    Bool_array.mapi t.filter ~f:(fun index bool ->
        if not bool
        then false
        else (
          let rec loop : type a. a Filter.t -> a -> bool =
           fun t fn ->
            match t with
            | Return -> fn
            | Apply (w, t) -> loop t (fn (Filter.get w index))
          in
          loop filter_ fn))
  in
  { columns = t.columns; filter }

module App = struct
  type nonrec 'a t = t -> index:int -> 'a

  let return a _df ~index:_ = a

  let apply t1 t2 df ~index =
    let t1 = t1 df ~index in
    let t2 = t2 df ~index in
    t1 t2

  let filter (t : bool t) df =
    let filter = Bool_array.mapi df.filter ~f:(fun index b -> b && t df ~index) in
    { columns = df.columns; filter }

  let int name df ~index =
    let column = Column.extract_exn (get_column_exn df name) Native_array.int in
    Column.get column index

  let float name df ~index =
    let column = Column.extract_exn (get_column_exn df name) Native_array.float in
    Column.get column index
end
