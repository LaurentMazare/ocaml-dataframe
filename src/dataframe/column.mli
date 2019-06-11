type ('a, 'b) t
type packed = P : _ t -> packed

val extract : packed -> ('a, 'b) Array_intf.t -> ('a, 'b) t option
val extract_exn : packed -> ('a, 'b) Array_intf.t -> ('a, 'b) t
val create : ('a, 'b) Array_intf.t -> 'a -> len:int -> ('a, 'b) t
val of_data : ('a, 'b) Array_intf.t -> 'b -> ('a, 'b) t
val of_array : ('a, 'b) Array_intf.t -> 'a array -> ('a, 'b) t
val copy : ?filter:Bool_array.t -> ('a, 'b) t -> ('a, 'b) t
val get : ('a, _) t -> int -> 'a
val set : ('a, _) t -> int -> 'a -> unit
val length : _ t -> int
val elt_name : _ t -> string
val get_string : _ t -> int -> string
val to_string : ?max_rows:int -> ?filter:Bool_array.t -> _ t -> string
val select : ('a, 'b) t -> indexes:int array -> ('a, 'b) t
val packed_copy : ?filter:Bool_array.t -> packed -> packed
val packed_length : packed -> int
val packed_elt_name : packed -> string
val packed_to_string : ?max_rows:int -> ?filter:Bool_array.t -> packed -> string
val packed_get_string : packed -> int -> string
val packed_select : packed -> indexes:int array -> packed
