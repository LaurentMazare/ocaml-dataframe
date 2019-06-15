(* TODO: automatic cast for [read] ?
*)
open Base

(** {3 CSV Reading and Writing } *)

(** [read ?columns filename] loads a dataframe from a CSV file.
    If [columns] is specified, only the named columns are used in the
    returned dataframe. If some columns are missing from the CSV file
    an error is returned.
*)
val read
  :  ?columns:(string * Array_intf.packed) list
  -> string
  -> [ `unfiltered ] Df.t Or_error.t

(** Similar to [read_exn] but raises exception on errors. *)
val read_exn
  :  ?columns:(string * Array_intf.packed) list
  -> string
  -> [ `unfiltered ] Df.t

(** [write df filename] writes dataframe [df] in CSV format. *)
val write : _ Df.t -> string -> unit Or_error.t

(** Similar to [write_exn] but raises exception on errors. *)
val write_exn : _ Df.t -> string -> unit
