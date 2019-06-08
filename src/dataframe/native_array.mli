module Make(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t
module MakeOption(E: Array_intf.Elt) : Array_intf.S with type Elt.t = E.t option

module Int : Array_intf.S with type Elt.t = int
module Float : Array_intf.S with type Elt.t = float
module String : Array_intf.S with type Elt.t = string
