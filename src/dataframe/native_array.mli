(** Generate column storage backed by standard arrays. *)
module Make(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t and type t = E.t array
module MakeOption(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t option and type t = E.t Base.Option_array.t

val int : (int, int array) Array_intf.t
val float : (float, float array) Array_intf.t
val string : (string, string array) Array_intf.t

val pint : Array_intf.packed
val pfloat : Array_intf.packed
val pstring : Array_intf.packed
