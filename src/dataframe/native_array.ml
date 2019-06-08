open Base

module Make (E : Array_intf.Elt) : Array_intf.S with type Elt.t = E.t = struct
  type t = E.t array

  module Elt = E

  let length = Array.length
  let get = Array.get
  let set = Array.set

  let type_id =
    Type_equal.Id.create ~name:(E.name ^ " native-array") (fun _ -> Sexp.Atom "opaque")
end

module MakeOption (E : Array_intf.Elt) : Array_intf.S with type Elt.t = E.t option =
struct
  type t = E.t Option_array.t

  module Elt = struct
    type t = E.t option

    let name = E.name ^ " option"
    let to_string = Option.value_map ~default:"" ~f:E.to_string

    let of_string = function
      | "" -> Some None
      | str -> Option.map (E.of_string str) ~f:Option.some
  end

  let length = Option_array.length
  let get = Option_array.get
  let set = Option_array.set

  let type_id =
    Type_equal.Id.create ~name:(E.name ^ " opt-array") (fun _ -> Sexp.Atom "opaque")
end

module Int = Make (struct
  type t = int

  let name = "int"
  let to_string = Int.to_string

  let of_string s =
    try Int.of_string s |> Option.some with
    | _ -> None
end)

module Float = Make (struct
  type t = float

  let name = "float"
  let to_string = Float.to_string

  let of_string s =
    try Float.of_string s |> Option.some with
    | _ -> None
end)

module String = Make (struct
  type t = string

  let name = "float"
  let to_string = Fn.id
  let of_string s = Some s
end)
