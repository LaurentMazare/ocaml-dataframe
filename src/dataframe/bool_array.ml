open Base

(* This is immutable so we can store num_set here. *)
type t =
  { data : Bytes.t
  ; length : int
  ; num_set : int
  }

let create b ~len:length =
  if length < 0
  then failwith "negative length"
  else if length > Sys.max_string_length * 8
  then Printf.failwithf "%d is above max-string-length" length ()
  else (
    let num_set = if b then length else 0 in
    let fill_value = Char.of_int_exn (if b then 255 else 0) in
    let data_length = (length + 7) / 8 in
    let data = Bytes.make data_length fill_value in
    { data; length; num_set })

let copy t = { data = Bytes.copy t.data; length = t.length; num_set = t.num_set }

let get t i =
  if i < 0
  then Printf.failwithf "negative index %d" i ()
  else if i >= t.length
  then Printf.failwithf "index above length %d >= %d" i t.length ()
  else (
    let byte = Bytes.get t.data (i lsr 3) |> Char.to_int in
    let index_in_byte = i land 7 in
    byte land (1 lsl index_in_byte) <> 0)

let num_set t = t.num_set
