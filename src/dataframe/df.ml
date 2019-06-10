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

module Csv = struct
  let read _filename = `not_implemented_yet
  let write _t _filename = `not_implemented_yet
end

let sort _t = `not_implemented_yet
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
          name ^ ":\n[\n" ^ Column.packed_to_string ~filter:t.filter column ^ "]")
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
  List.map (delim :: header :: delim :: rows) ~f:(fun row ->
      Array.mapi row ~f:(fun i cell ->
          let col_len = 2 + max_len_per_column.(i) in
          let pad = col_len - String.length cell in
          String.make pad ' ' ^ cell)
      |> String.concat_array)

let copy t =
  { columns = Map.map t.columns ~f:Column.(packed_copy ~filter:t.filter)
  ; filter = Bool_array.create true ~len:(Bool_array.num_set t.filter)
  }

(* Applicative module for filtering, mapping, etc. *)
module Row_map = struct
  type nonrec 'a t_ = t -> (index:int -> 'a) Staged.t

  module A = Applicative.Make (struct
    type 'a t = 'a t_

    let return a _df = Staged.stage (fun ~index:_ -> a)

    let apply t1 t2 df =
      let t1 = Staged.unstage (t1 df) in
      let t2 = Staged.unstage (t2 df) in
      Staged.stage (fun ~index -> (t1 ~index) (t2 ~index))

    let map = `Define_using_apply
  end)

  module App = struct
    type 'a t = 'a t_

    include A
  end

  module Open_on_rhs_intf = struct
    module type S = Applicative.S
  end

  include App
  include Applicative.Make_let_syntax (App) (Open_on_rhs_intf) (App)

  let column mod_ name df =
    let column = Column.extract_exn (get_column_exn df name) mod_ in
    Staged.stage (fun ~index -> Column.get column index)

  let int = column Native_array.int
  let float = column Native_array.float
  let string = column Native_array.string
end

let filter t (f : bool Row_map.t) =
  let f = Staged.unstage (f t) in
  let filter = Bool_array.mapi t.filter ~f:(fun index b -> b && f ~index) in
  { columns = t.columns; filter }
