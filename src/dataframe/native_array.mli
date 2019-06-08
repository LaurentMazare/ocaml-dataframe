module Make(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t
module MakeOption(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t option

val int : (module Array_intf.S with type Elt.t = int and type t = int array)
val float : (module Array_intf.S with type Elt.t = float and type t = float array)
val string : (module Array_intf.S with type Elt.t = string and type t = string array)
