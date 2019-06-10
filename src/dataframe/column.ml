open! Base

type ('a, 'b) t =
  { mod_ : (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  ; data : 'b
  }

type packed = P : _ t -> packed

let create : type a b.
    (module Array_intf.S with type Elt.t = a and type t = b) -> a -> len:int -> (a, b) t
  =
 fun mod_ v ~len ->
  let (module M) = mod_ in
  { mod_; data = M.create v ~len }

let of_array : type a b.
    (module Array_intf.S with type Elt.t = a and type t = b) -> a array -> (a, b) t
  =
 fun mod_ vs ->
  let (module M) = mod_ in
  { mod_; data = M.of_array vs }

let copy : type a b. ?filter:Bool_array.t -> (a, b) t -> (a, b) t =
 fun ?filter t ->
  let (module M) = t.mod_ in
  { mod_ = t.mod_; data = M.copy ?filter t.data }

let get : type a b. (a, b) t -> int -> a =
 fun t i ->
  let (module M) = t.mod_ in
  M.get t.data i

let set : type a b. (a, b) t -> int -> a -> unit =
 fun t i v ->
  let (module M) = t.mod_ in
  M.set t.data i v

let extract : type a b.
    packed -> (module Array_intf.S with type Elt.t = a and type t = b) -> (a, b) t option
  =
 fun (P t) (module M) ->
  let (module M') = t.mod_ in
  match Type_equal.Id.same_witness M.type_id M'.type_id with
  | Some T -> Some t
  | None -> None

let extract_exn : type a b.
    packed -> (module Array_intf.S with type Elt.t = a and type t = b) -> (a, b) t
  =
 fun t m -> Option.value_exn (extract t m)

let length : type a b. (a, b) t -> int =
 fun t ->
  let (module M) = t.mod_ in
  M.length t.data

let elt_name : type a b. (a, b) t -> string =
 fun t ->
  let (module M) = t.mod_ in
  M.Elt.name

let get_string : type a b. (a, b) t -> int -> string =
 fun t i ->
  let (module M) = t.mod_ in
  M.get t.data i |> M.Elt.to_string

let to_string : type a b. ?max_rows:int -> ?filter:Bool_array.t -> (a, b) t -> string =
 fun ?(max_rows = 10) ?filter t ->
  let (module M) = t.mod_ in
  let length = M.length t.data in
  Option.iter filter ~f:(fun filter ->
      if Bool_array.length filter <> length
      then
        Printf.failwithf
          "incoherent filter size %d <> %d"
          (Bool_array.length filter)
          length
          ());
  let rec loop n ~index acc =
    if n = 0 || index = length - 1
    then List.rev acc
    else (
      let filter_ok =
        Option.value_map filter ~default:true ~f:(fun filter ->
            Bool_array.get filter index)
      in
      let rem, acc =
        if filter_ok
        then n - 1, (M.get t.data index |> M.Elt.to_string) :: acc
        else n, acc
      in
      loop rem acc ~index:(index + 1))
  in
  let data = loop max_rows ~index:0 [] in
  String.concat data ~sep:"\n"

let packed_copy ?filter (P t) = P (copy ?filter t)
let packed_length (P t) = length t
let packed_elt_name (P t) = elt_name t
let packed_to_string ?max_rows ?filter (P t) = to_string ?max_rows ?filter t
let packed_get_string (P t) i = get_string t i
