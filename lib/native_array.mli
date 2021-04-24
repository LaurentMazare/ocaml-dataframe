(** Generate column storage backed by standard arrays. *)
module Make (E : Array_intf.Elt) :
  Array_intf.S with type Elt.t = E.t and type t = E.t array

module MakeOption (E : Array_intf.Elt) :
  Array_intf.S with type Elt.t = E.t option and type t = E.t Base.Option_array.t

val intarr : (int, int array) Array_intf.t
val floatarr : (float, float array) Array_intf.t
val stringarr : (string, string array) Array_intf.t
val pintarr : Array_intf.packed
val pfloatarr : Array_intf.packed
val pstringarr : Array_intf.packed

open Base
open Df

module type BASICTYPE = sig
  type elt
  type comparator_witness

  val min : _ t -> name:string -> elt option
  val max : _ t -> name:string -> elt option
  val value_counts : _ t -> name:string -> (elt, int, comparator_witness) Map.t
end

module Float : sig
  include
    BASICTYPE
      with type elt := float
       and type comparator_witness := Float.comparator_witness

  val sum : _ t -> name:string -> float
  val mean : _ t -> name:string -> float option
end

module Int : sig
  include
    BASICTYPE with type elt := int and type comparator_witness := Int.comparator_witness

  val sum : _ t -> name:string -> int
  val mean : _ t -> name:string -> float option
end

module String :
  BASICTYPE
    with type elt := string
     and type comparator_witness := String.comparator_witness
