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

let to_string : type a b. (a, b) t -> string =
 fun t ->
  let (module M) = t.mod_ in
  let length = M.length t.data in
  let data =
    if length < 10
    then List.init length ~f:(M.get t.data)
    else
      List.init 5 ~f:(M.get t.data)
      @ List.init 5 ~f:(fun i -> M.get t.data (length - 5 + i))
  in
  List.map data ~f:M.Elt.to_string |> String.concat ~sep:"\n"

let packed_length (P t) = length t
let packed_elt_name (P t) = elt_name t
let packed_to_string (P t) = to_string t
let packed_get_string (P t) i = get_string t i
