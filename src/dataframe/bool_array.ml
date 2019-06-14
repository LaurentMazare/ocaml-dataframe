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

let length t = t.length

let get_ data i ~length =
  if i < 0
  then Printf.failwithf "negative index %d" i ()
  else if i >= length
  then Printf.failwithf "index above length %d >= %d" i length ()
  else (
    let byte = Bytes.get data (i lsr 3) |> Char.to_int in
    let index_in_byte = i land 7 in
    byte land (1 lsl index_in_byte) <> 0)

let get t i = get_ t.data i ~length:t.length
let num_set t = t.num_set

let iteri_ data ~f ~length =
  let bytes_length = Bytes.length data in
  for byte_index = 0 to bytes_length - 1 do
    let byte = Bytes.unsafe_get data byte_index |> Char.to_int in
    let bit_offset = 8 * byte_index in
    let bits_used = if byte_index <> bytes_length - 1 then 8 else length land 7 in
    let bits_used = if bits_used = 0 then 8 else bits_used in
    for i = 0 to bits_used - 1 do
      let bit_set = 1 lsl i in
      f (bit_offset + i) (byte land bit_set <> 0)
    done
  done

let iteri t ~f = iteri_ t.data ~f ~length:t.length

let mapi t ~f =
  let bytes_length = Bytes.length t.data in
  let num_set = ref 0 in
  let data = Bytes.make bytes_length (Char.of_int_exn 0) in
  for byte_index = 0 to bytes_length - 1 do
    let byte = Bytes.unsafe_get t.data byte_index |> Char.to_int in
    let bit_offset = 8 * byte_index in
    let bits_used = if byte_index <> bytes_length - 1 then 8 else t.length land 7 in
    let bits_used = if bits_used = 0 then 8 else bits_used in
    let v = ref 0 in
    for i = 0 to bits_used - 1 do
      let bit_set = 1 lsl i in
      if f (bit_offset + i) (byte land bit_set <> 0)
      then (
        v := !v lor bit_set;
        Int.incr num_set)
    done;
    if !v <> 0 then Bytes.unsafe_set data byte_index (Char.of_int_exn !v)
  done;
  { data; length = t.length; num_set = !num_set }

let indexes t ~value =
  let indexes_len = if value then t.num_set else t.length - t.num_set in
  let indexes = Array.create (-1) ~len:indexes_len in
  let indexes_i = ref 0 in
  iteri t ~f:(fun i b ->
      if Bool.( = ) b value
      then (
        indexes.(!indexes_i) <- i;
        Int.incr indexes_i));
  indexes

(* We could also use a phantom type rather than a separate module. *)
module Mutable = struct
  type immutable = t

  type t =
    { data : Bytes.t
    ; length : int
    }

  let create v ~len =
    let t = create v ~len in
    { data = t.data; length = t.length }

  let get t i = get_ t.data i ~length:t.length

  let set t i value =
    if i < 0
    then Printf.failwithf "negative index %d" i ()
    else if i >= t.length
    then Printf.failwithf "index above length %d >= %d" i t.length ()
    else (
      let byte_index = i lsr 3 in
      let byte = Bytes.get t.data byte_index |> Char.to_int in
      let index_in_byte = i land 7 in
      let bit_set = 1 lsl index_in_byte in
      let byte = if value then byte lor bit_set else byte land lnot bit_set in
      Bytes.unsafe_set t.data byte_index (Char.of_int_exn byte))

  let length t = t.length

  let finish : t -> immutable =
   fun t ->
    let num_set = ref 0 in
    iteri_ t.data ~length:t.length ~f:(fun _ b -> if b then Int.incr num_set);
    { data = Bytes.copy t.data; length = t.length; num_set = !num_set }
end
