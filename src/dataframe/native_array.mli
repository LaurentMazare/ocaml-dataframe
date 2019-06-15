module Make(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t
module MakeOption(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t option

val int : (int, int array) Array_intf.t
val float : (float, float array) Array_intf.t
val string : (string, string array) Array_intf.t

val pint : Array_intf.packed
val pfloat : Array_intf.packed
val pstring : Array_intf.packed
