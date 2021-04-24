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

let of_rows1 (name1, mod1) elements =
  let column = Column.of_array mod1 (Array.of_list elements) in
  create [ name1, P column ]

let of_rows1_exn col1 elements = of_rows1 col1 elements |> Or_error.ok_exn

let of_rows2 (n1, mod1) (n2, mod2) elements =
  let get mod_ ~f = Column.P (Column.of_array mod_ (Array.of_list_map elements ~f)) in
  create [ n1, get mod1 ~f:fst; n2, get mod2 ~f:snd ]

let of_rows2_exn c1 c2 elements = of_rows2 c1 c2 elements |> Or_error.ok_exn

let of_rows3 (n1, mod1) (n2, mod2) (n3, mod3) elements =
  let get mod_ ~f = Column.P (Column.of_array mod_ (Array.of_list_map elements ~f)) in
  create
    [ n1, get mod1 ~f:(fun (e1, _, _) -> e1)
    ; n2, get mod2 ~f:(fun (_, e2, _) -> e2)
    ; n3, get mod3 ~f:(fun (_, _, e3) -> e3)
    ]

let of_rows3_exn c1 c2 c3 elements = of_rows3 c1 c2 c3 elements |> Or_error.ok_exn

let of_rows4 (n1, mod1) (n2, mod2) (n3, mod3) (n4, mod4) elements =
  let get mod_ ~f = Column.P (Column.of_array mod_ (Array.of_list_map elements ~f)) in
  create
    [ n1, get mod1 ~f:(fun (e1, _, _, _) -> e1)
    ; n2, get mod2 ~f:(fun (_, e2, _, _) -> e2)
    ; n3, get mod3 ~f:(fun (_, _, e3, _) -> e3)
    ; n4, get mod4 ~f:(fun (_, _, _, e4) -> e4)
    ]

let of_rows4_exn c1 c2 c3 c4 elements = of_rows4 c1 c2 c3 c4 elements |> Or_error.ok_exn

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

let to_aligned_rows ?max_length (type a) (t : a t) =
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
  let target_length l =
    match max_length with
    | None -> l
    | Some max_length -> Int.min l max_length
  in
  let rows =
    match t.filter with
    | No_filter len -> List.init (target_length len) ~f:(fun index -> row ~index)
    | Filter filter ->
      List.init
        (Bool_array.length filter |> target_length)
        ~f:(fun index ->
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

let print ?(out_channel = Stdio.Out_channel.stdout) ?max_length (type a) (t : a t) =
  Stdio.Out_channel.output_lines out_channel (to_aligned_rows ?max_length t);
  Stdio.Out_channel.flush out_channel

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
        | Some column -> First (name, column)
        | None -> Second name)
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
  module AB = struct
    type 'a t = packed -> (index:int -> 'a) Staged.t

    let return x _df = Staged.stage (fun ~index:_ -> x)

    let apply f x df =
      let f = Staged.unstage (f df) in
      let x = Staged.unstage (x df) in
      Staged.stage (fun ~index -> (f ~index) (x ~index))

    let map = `Define_using_apply
  end

  include AB

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

  module A = Applicative.Make (AB)
  include A

  let ( let+ ) x f = map x ~f
  let ( and+ ) = both
end

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

let iter : type a. a t -> unit R.t -> unit =
 fun t f ->
  if length t = 0
  then ()
  else (
    let f = Staged.unstage (f (P t)) in
    iter_row t ~f:(fun index -> f ~index))

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

let map_one
    : type a b c d.
      _ t
      -> name:string
      -> src:(c, d) Array_intf.t
      -> dst:(a, b) Array_intf.t
      -> f:(c -> a)
      -> (a, b) Column.t
  =
 fun t ~name ~src ~dst ~f ->
  let (P column) = get_column_exn t name in
  let (module M) = Column.mod_ column in
  let (module M') = src in
  let (module M_dst) = dst in
  match Type_equal.Id.same_witness M.type_id M'.type_id with
  | Some T ->
    Array.init (Column.length column) ~f:(fun i -> Column.get column i |> f)
    |> M_dst.of_array
    |> Column.of_data dst
  | None ->
    Printf.failwithf
      "type mismatch for column %s (expected %s got %s)"
      name
      M.Elt.name
      M'.Elt.name
      ()

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

let concat ts =
  match ts with
  | [] -> Or_error.error_string "df.concat: empty input list"
  | hd :: _ ->
    let col_types =
      Map.map hd.columns ~f:(fun (Column.P col) -> Array_intf.P (Column.mod_ col))
    in
    let col_types_list = Map.to_alist col_types in
    let total_length = List.sum (module Int) ts ~f:length in
    let errors =
      List.filter_mapi ts ~f:(fun i t ->
          let additional_keys =
            Map.keys t.columns |> List.filter ~f:(fun n -> not (Map.mem col_types n))
          in
          match additional_keys with
          | [] ->
            let errors =
              List.filter_map col_types_list ~f:(fun (name, Array_intf.P mod_) ->
                  let (module M) = mod_ in
                  match Map.find t.columns name with
                  | None -> Printf.sprintf "missing column %s" name |> Option.some
                  | Some (P col) ->
                    let (module M') = Column.mod_ col in
                    (match Type_equal.Id.same_witness M.type_id M'.type_id with
                    | Some T -> None
                    | None ->
                      Printf.sprintf "different types for column %s" name |> Option.some))
            in
            (match errors with
            | [] -> None
            | errors ->
              Printf.sprintf
                "errors in dataframe %d: %s"
                i
                (String.concat errors ~sep:",")
              |> Error.of_string
              |> Option.some)
          | columns ->
            Printf.sprintf
              "additional columns in dataframe %d: %s"
              i
              (String.concat columns ~sep:",")
            |> Error.of_string
            |> Option.some)
    in
    (match errors with
    | [] ->
      let columns =
        Map.mapi col_types ~f:(fun ~key:name ~data:(Array_intf.P mod_) ->
            let (module M) = mod_ in
            let f : type a. a t -> Bool_array.t option * (M.Elt.t, M.t) Column.t =
             fun t ->
              let (Column.P column) = Map.find_exn t.columns name in
              let (module M') = Column.mod_ column in
              match Type_equal.Id.same_witness M.type_id M'.type_id with
              | None -> assert false
              | Some T ->
                let filter =
                  match t.filter with
                  | Filter f -> Some f
                  | No_filter _ -> None
                in
                filter, column
            in
            let columns = List.map ts ~f in
            Column.P (Column.concat mod_ columns))
      in
      Ok { columns; filter = Filter.No_filter total_length }
    | errors -> Error (Error.of_list errors))

let concat_exn ts = concat ts |> Or_error.ok_exn

let to_filtered : type a. a t -> [ `filtered ] t =
 fun t ->
  match t.filter with
  | Filter _ -> t
  | No_filter len ->
    { columns = t.columns; filter = Filter (Bool_array.create true ~len) }

let incr = function
  | None -> Some 1
  | Some v -> Some (v + 1)

(* TODO: the [Float_] and [Int_] modules are very similar, use a functor
   instead ([M.Elt] contains the [compare] function needed for [min] and
   [max]) ?
*)
module Float_ = struct
  let sum (type a) (t : a t) ~name =
    let f =
      let open R in
      let+ v = float name in
      fun acc -> acc +. v
    in
    fold t ~init:0. ~f

  let mean (type a) (t : a t) ~name =
    let sum = sum t ~name in
    let nrows = length t in
    if nrows = 0 then None else Some (sum /. Float.of_int nrows)

  let min (type a) (t : a t) ~name = reduce t (R.float name) ~f:Float.min
  let max (type a) (t : a t) ~name = reduce t (R.float name) ~f:Float.max

  let value_counts (type a) (t : a t) ~name =
    let f =
      let open R in
      let+ v = float name in
      fun acc -> Map.change acc v ~f:incr
    in
    fold t ~init:(Map.empty (module Float)) ~f
end

module Int_ = struct
  let sum (type a) (t : a t) ~name =
    let f =
      let open R in
      let+ v = int name in
      fun acc -> acc + v
    in
    fold t ~init:0 ~f

  let mean (type a) (t : a t) ~name =
    let sum = sum t ~name in
    let nrows = length t in
    if nrows = 0 then None else Some (Float.of_int sum /. Float.of_int nrows)

  let min (type a) (t : a t) ~name = reduce t (R.int name) ~f:Int.min
  let max (type a) (t : a t) ~name = reduce t (R.int name) ~f:Int.max

  let value_counts (type a) (t : a t) ~name =
    let f =
      let open R in
      let+ v = int name in
      fun acc -> Map.change acc v ~f:incr
    in
    fold t ~init:(Map.empty (module Int)) ~f
end

module String_ = struct
  let min (type a) (t : a t) ~name = reduce t (R.string name) ~f:String.min
  let max (type a) (t : a t) ~name = reduce t (R.string name) ~f:String.max

  let value_counts (type a) (t : a t) ~name =
    let f =
      let open R in
      let+ v = string name in
      fun acc -> Map.change acc v ~f:incr
    in
    fold t ~init:(Map.empty (module String)) ~f
end

module type BASICTYPE = sig
  type elt
  type comparator_witness

  val min : _ t -> name:string -> elt option
  val max : _ t -> name:string -> elt option
  val value_counts : _ t -> name:string -> (elt, int, comparator_witness) Map.t
end

module Float = Float_
module Int = Int_
module String = String_
