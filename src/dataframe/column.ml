open! Base

type ('a, 'b) t =
  { mod_ : (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  ; data : 'b
  ; name : string
  }

type packed = P : _ t -> packed

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

let packed_length (P t) = length t
let packed_elt_name (P t) = elt_name t
