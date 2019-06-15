(* TODO: handle explicit headers in [Csv.read] ?
   TODO: automatic cast for [Csv.read].
*)
open Base

(** {3 CSV Reading and Writing } *)

val read : string -> [ `unfiltered ] Df.t Or_error.t
val read_exn : string -> [ `unfiltered ] Df.t
val write : _ Df.t -> string -> unit Or_error.t
val write_exn : _ Df.t -> string -> unit
