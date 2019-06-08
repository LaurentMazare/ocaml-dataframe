open Base

(* TODO: use a compact version of this. *)
type t = bool array

let create = Array.create
let copy = Array.copy
let get = Array.get
let num_set = Array.sum (module Int) ~f:(fun b -> if b then 1 else 0)
