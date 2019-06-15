open Base

type 'a t =
  { columns : (string, Column.packed, String.comparator_witness) Map.t
  ; filter : 'a Filter.t
  }

type packed = P : 'a t -> packed

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
      | `Ok columns -> Ok { columns; filter = No_filter first_column_length }
      | `Duplicate_key column_name ->
        Or_error.errorf "duplicate column name %s" column_name)

let create_exn columns = create columns |> Or_error.ok_exn

let iter_row (type a) (t : a t) ~f =
  match t.filter with
  | No_filter len ->
    for i = 0 to len - 1 do
      f i
    done
  | Filter filter -> Bool_array.iteri filter ~f:(fun i b -> if b then f i)

let get_column t column_name = Map.find t.columns column_name

let get_column_exn t column_name =
  match get_column t column_name with
  | None -> Printf.failwithf "cannot find column %s" column_name ()
  | Some column -> column

let column_names t = Map.keys t.columns
let named_columns t = Map.to_alist t.columns
let filter_ t = t.filter

let column_types t =
  named_columns t |> List.map ~f:(fun (_key, column) -> Column.packed_elt_name column)

let to_string (type a) ?(headers_only = false) (t : a t) =
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
          let filter =
            match t.filter with
            | No_filter _ -> None
            | Filter f -> Some f
          in
          name ^ ":\n[\n" ^ Column.packed_to_string ?filter column ^ "]")
      |> String.concat ~sep:"\n"
    in
    header ^ "\n---\n" ^ values)

(* This returns the filtered length. *)
let length (type a) (t : a t) =
  match t.filter with
  | No_filter len -> len
  | Filter f -> Bool_array.num_set f

let unfiltered_length (type a) (t : a t) =
  match t.filter with
  | No_filter len -> len
  | Filter f -> Bool_array.length f

let num_rows = length
let num_cols t = Map.length t.columns

let to_aligned_rows (type a) (t : a t) =
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
  let row ~index =
    Array.mapi named_columns ~f:(fun i (_, column) ->
        let str = Column.packed_get_string column index |> escape in
        max_len_per_column.(i) <- max max_len_per_column.(i) (String.length str);
        str)
  in
  let rows =
    match t.filter with
    | No_filter len -> List.init len ~f:(fun index -> row ~index)
    | Filter filter ->
      List.init (Bool_array.length filter) ~f:(fun index ->
          if Bool_array.get filter index then row ~index |> Option.some else None)
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

let print ?(out_channel = Stdio.Out_channel.stdout) (type a) (t : a t) =
  Stdio.Out_channel.output_lines out_channel (to_aligned_rows t)

let copy (type a) (t : a t) =
  let filter, len =
    match t.filter with
    | No_filter len -> None, len
    | Filter filter -> Some filter, Bool_array.num_set filter
  in
  { columns = Map.map t.columns ~f:Column.(packed_copy ?filter); filter = No_filter len }

let filter_columns (type a) (t : a t) ~names =
  let names = List.dedup_and_sort names ~compare:String.compare in
  let columns, unknown_names =
    List.partition_map names ~f:(fun name ->
        match Map.find t.columns name with
        | Some column -> `Fst (name, column)
        | None -> `Snd name)
  in
  if not (List.is_empty unknown_names)
  then
    Or_error.errorf
      "some columns cannot be found: %s not in %s"
      (String.concat unknown_names ~sep:",")
      (Map.keys t.columns |> String.concat ~sep:",")
  else (
    let columns = Map.of_alist_exn (module String) columns in
    Ok { columns; filter = t.filter })

let filter_columns_exn (type a) (t : a t) ~names =
  filter_columns t ~names |> Or_error.ok_exn

(* Applicative module for filtering, mapping, etc. *)
module R = struct
  type nonrec 'a t_ = packed -> (index:int -> 'a) Staged.t

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

  (* We probably don't need to pass a full array_intf here, a witness
     for the element type would be enough.
  *)
  let column : type a b. (a, b) Array_intf.t -> string -> a t =
   fun mod_ name (P df) ->
    let column =
      let column = get_column_exn df name in
      match Column.extract column mod_ with
      | Some column -> column
      | None ->
        let (module M) = mod_ in
        Printf.failwithf
          "type mismatch for column %s (expected %s got %s)"
          name
          M.Elt.name
          (Column.packed_elt_name column)
          ()
    in
    Staged.stage (fun ~index -> Column.get column index)

  let int = column Native_array.int
  let float = column Native_array.float
  let string = column Native_array.string
end

open R.Let_syntax

let filter (type a) (t : a t) (f : bool R.t) =
  let f = Staged.unstage (f (P t)) in
  let filter =
    match t.filter with
    | No_filter len -> Bool_array.create true ~len
    | Filter filter -> filter
  in
  let filter = Bool_array.mapi filter ~f:(fun index b -> b && f ~index) in
  { columns = t.columns; filter = Filter filter }

let map : type a b c. c t -> (a, b) Array_intf.t -> a R.t -> (a, b) Column.t =
 fun t mod_ f ->
  let (module M) = mod_ in
  if length t = 0
  then Column.of_array mod_ [||]
  else (
    let f = Staged.unstage (f (P t)) in
    let new_index = ref 0 in
    (* Lazy creation of the array as we need to know the first value
       to be able to create this. *)
    let data = ref None in
    iter_row t ~f:(fun index ->
        let v = f ~index in
        let data =
          match !data with
          | None ->
            let d = M.create v ~len:(length t) in
            data := Some d;
            d
          | Some data -> data
        in
        M.set data !new_index v;
        Int.incr new_index);
    match !data with
    | None -> Column.of_array mod_ [||]
    | Some data -> Column.of_data mod_ data)

let add_column t ~name column =
  match t.filter with
  | No_filter len ->
    if Map.mem t.columns name
    then Or_error.errorf "column %s already exists in dataframe" name
    else if len <> Column.length column
    then Or_error.errorf "length mismatch %d <> %d" len (Column.length column)
    else (
      let columns = Map.add_exn t.columns ~key:name ~data:(P column) in
      Ok { columns; filter = t.filter })

let add_column_exn t ~name column = add_column t ~name column |> Or_error.ok_exn
let map_and_add_column t ~name mod_ f = add_column t ~name (map t mod_ f)
let map_and_add_column_exn t ~name mod_ f = add_column_exn t ~name (map t mod_ f)

let sort (type a) (t : a t) f ~compare =
  let indexes =
    let f = Staged.unstage (f (P t)) in
    match t.filter with
    | No_filter len -> Array.init len ~f:(fun index -> f ~index, index)
    | Filter filter ->
      Bool_array.indexes filter ~value:true |> Array.map ~f:(fun index -> f ~index, index)
  in
  Array.sort indexes ~compare:(fun (a1, _) (a2, _) -> compare a1 a2);
  let indexes = Array.map indexes ~f:snd in
  let columns =
    Map.map t.columns ~f:(fun packed_column ->
        Column.packed_select packed_column ~indexes)
  in
  { columns; filter = No_filter (Array.length indexes) }

let sort_by (type a) ?(reverse = false) (t : a t) ~name =
  let (P column) = get_column_exn t name in
  let (module M) = Column.mod_ column in
  let f _ = Staged.stage (fun ~index -> Column.get column index) in
  let compare = if reverse then fun t1 t2 -> -M.Elt.compare t1 t2 else M.Elt.compare in
  sort t f ~compare

let group (type a) (t : a t) f =
  let len = unfiltered_length t in
  let f = Staged.unstage (f (P t)) in
  let values_and_filters = Hashtbl.Poly.create () in
  iter_row t ~f:(fun index ->
      let v = f ~index in
      let filter =
        Hashtbl.find_or_add values_and_filters v ~default:(fun () ->
            Bool_array.Mutable.create false ~len)
      in
      Bool_array.Mutable.set filter index true);
  Hashtbl.to_alist values_and_filters
  |> List.map ~f:(fun (key, filter) ->
         let filter = Bool_array.Mutable.finish filter in
         key, { columns = t.columns; filter = Filter filter })

let fold (type a) (t : a t) ~init ~f =
  let f = Staged.unstage (f (P t)) in
  let acc = ref init in
  iter_row t ~f:(fun index -> acc := f ~index !acc);
  !acc

let reduce (type a) (t : a t) row_f ~f =
  let row_f = Staged.unstage (row_f (P t)) in
  let acc = ref None in
  iter_row t ~f:(fun index ->
      let v = row_f ~index in
      let v =
        match !acc with
        | None -> Some v
        | Some acc -> Some (f acc v)
      in
      acc := v);
  !acc

(* TODO: the [Float_] and [Int_] modules are very similar, use a functor
   instead ([M.Elt] contains the [compare] function needed for [min] and
   [max] ?
*)
module Float_ = struct
  let sum (type a) (t : a t) ~name =
    let f =
      [%map_open
        let v = R.float name in
        fun acc -> acc +. v]
    in
    fold t ~init:0. ~f

  let mean (type a) (t : a t) ~name =
    let sum = sum t ~name in
    let nrows = length t in
    if nrows = 0 then None else Some (sum /. Float.of_int nrows)

  let min (type a) (t : a t) ~name = reduce t (R.float name) ~f:Float.min
  let max (type a) (t : a t) ~name = reduce t (R.float name) ~f:Float.max
end

module Int_ = struct
  let sum (type a) (t : a t) ~name =
    let f =
      [%map_open
        let v = R.int name in
        fun acc -> acc + v]
    in
    fold t ~init:0 ~f

  let mean (type a) (t : a t) ~name =
    let sum = sum t ~name in
    let nrows = length t in
    if nrows = 0 then None else Some (Float.of_int sum /. Float.of_int nrows)

  let min (type a) (t : a t) ~name = reduce t (R.int name) ~f:Int.min
  let max (type a) (t : a t) ~name = reduce t (R.int name) ~f:Int.max
end

module Float = Float_
module Int = Int_
