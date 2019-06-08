open Base

(* Element interface. *)
module type Elt = sig
  type t

  val name : string
  val to_string : t -> string

  (* [of_string str] is used when parsing csv files. *)
  val of_string : string -> t option
end

(* Arrays could be based on different storage types, e.g.
   array, bigarray, ...
*)
module type S = sig
  type t

  module Elt : Elt

  val length : t -> int
  val get : t -> int -> Elt.t
  val set : t -> int -> Elt.t -> unit
  val type_id : (t * Elt.t) Type_equal.Id.t
end
