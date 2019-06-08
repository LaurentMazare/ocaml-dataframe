type ('a, 'b) t
type packed = P : _ t -> packed

val extract
  :  packed
  -> (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  -> ('a, 'b) t option

val get : ('a, _) t -> int -> 'a
val set : ('a, _) t -> int -> 'a -> unit
val length : _ t -> int
val elt_name : _ t -> string
val packed_length : packed -> int
val packed_elt_name : packed -> string
