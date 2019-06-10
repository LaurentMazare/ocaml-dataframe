(* Immutable bool array. *)
type t

val create : bool -> len:int -> t
val copy : t -> t
val get : t -> int -> bool
val num_set : t -> int
val length : t -> int
val mapi : t -> f:(int -> bool -> bool) -> t
val iteri : t -> f:(int -> bool -> unit) -> unit
