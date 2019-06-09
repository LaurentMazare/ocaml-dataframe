type ('a, 'b) t
type packed = P : _ t -> packed

val extract
  :  packed
  -> (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  -> ('a, 'b) t option

val extract_exn
  :  packed
  -> (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  -> ('a, 'b) t

val create
  :  (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  -> 'a
  -> len:int
  -> ('a, 'b) t

val of_array
  :  (module Array_intf.S with type Elt.t = 'a and type t = 'b)
  -> 'a array
  -> ('a, 'b) t

val copy : ?filter:Bool_array.t -> ('a, 'b) t -> ('a, 'b) t
val get : ('a, _) t -> int -> 'a
val set : ('a, _) t -> int -> 'a -> unit
val length : _ t -> int
val elt_name : _ t -> string
val get_string : _ t -> int -> string
val to_string : _ t -> string
val packed_copy : ?filter:Bool_array.t -> packed -> packed
val packed_length : packed -> int
val packed_elt_name : packed -> string
val packed_to_string : packed -> string
val packed_get_string : packed -> int -> string
