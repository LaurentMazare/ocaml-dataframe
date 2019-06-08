open Base

type t

val create : (string * Column.packed) list -> t Or_error.t
val create_exn : (string * Column.packed) list -> t
val read_csv : string -> t
val get_column : t -> string -> Column.packed option
val get_column_exn : t -> string -> Column.packed
val column_names : t -> string list
val named_columns : t -> (string * Column.packed) list
val to_string : ?headers_only:bool -> t -> string

(** [length t] returns the number of rows in t. *)
val length : t -> int

val num_rows : t -> int
val num_cols : t -> int
