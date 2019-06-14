(* Immutable bool array. *)
type t

val create : bool -> len:int -> t
val get : t -> int -> bool

(* The number of elements of the array set to true. *)
val num_set : t -> int

(* The indexes for a given value. *)
val indexes : t -> value:bool -> int array
val length : t -> int
val mapi : t -> f:(int -> bool -> bool) -> t
val iteri : t -> f:(int -> bool -> unit) -> unit

module Mutable : sig
  type immutable = t
  type t

  val create : bool -> len:int -> t
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
  val length : t -> int
  val finish : t -> immutable
end
