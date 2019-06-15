open Base

module Make (E : Array_intf.Elt) :
  Array_intf.S with type Elt.t = E.t and type t = E.t array = struct
  type t = E.t array

  module Elt = E

  let create v ~len = Array.create v ~len
  let of_array = Fn.id

  let copy ?filter t =
    match filter with
    | None -> Array.copy t
    | Some filter ->
      let num_set = Bool_array.num_set filter in
      if num_set = 0
      then [||]
      else (
        let res = Array.create t.(0) ~len:num_set in
        let res_index = ref 0 in
        Array.iteri t ~f:(fun i v ->
            if Bool_array.get filter i
            then (
              res.(!res_index) <- v;
              Int.incr res_index));
        res)

  let length = Array.length
  let get = Array.get
  let set = Array.set

  let type_id =
    Type_equal.Id.create ~name:(E.name ^ " native-array") (fun _ -> Sexp.Atom "opaque")
end

module MakeOption (E : Array_intf.Elt) :
  Array_intf.S with type Elt.t = E.t option and type t = E.t Option_array.t = struct
  type t = E.t Option_array.t

  module Elt = struct
    type t = E.t option

    let compare t1 t2 =
      match t1, t2 with
      | Some t1, Some t2 -> E.compare t1 t2
      | None, Some _ -> 1
      | Some _, None -> -1
      | None, None -> 0

    let name = E.name ^ " option"
    let to_string = Option.value_map ~default:"" ~f:E.to_string

    let of_string = function
      | "" -> Some None
      | str -> Option.map (E.of_string str) ~f:Option.some
  end

  let create v ~len = Option_array.init len ~f:(Fn.const v)
  let of_array vs = Option_array.init (Array.length vs) ~f:(fun i -> vs.(i))

  let copy ?filter t =
    match filter with
    | None -> Option_array.copy t
    | Some filter ->
      let num_set = Bool_array.num_set filter in
      if num_set = 0
      then Option_array.empty
      else (
        let res = Option_array.create ~len:num_set in
        let res_index = ref 0 in
        for i = 0 to Option_array.length t - 1 do
          if Bool_array.get filter i
          then (
            Option_array.set res !res_index (Option_array.get res i);
            Int.incr res_index)
        done;
        res)

  let length = Option_array.length
  let get = Option_array.get
  let set = Option_array.set

  let type_id =
    Type_equal.Id.create ~name:(E.name ^ " opt-array") (fun _ -> Sexp.Atom "opaque")
end

module Int = Make (struct
  include Int

  let name = "int"
  let to_string = Int.to_string

  let of_string s =
    try Int.of_string s |> Option.some with
    | _ -> None
end)

module Float = Make (struct
  include Float

  let name = "float"
  let to_string = Float.to_string

  let of_string s =
    try Float.of_string s |> Option.some with
    | _ -> None
end)

module String = Make (struct
  include String

  let name = "string"
  let to_string = Fn.id
  let of_string s = Some s
end)

let int = (module Int : Array_intf.S with type Elt.t = int and type t = int array)

let float =
  (module Float : Array_intf.S with type Elt.t = float and type t = float array)

let string =
  (module String : Array_intf.S with type Elt.t = string and type t = string array)

let pint = Array_intf.P int
let pfloat = Array_intf.P float
let pstring = Array_intf.P string
