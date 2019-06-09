open Base

type t

val create : (string * Column.packed) list -> t Or_error.t
val create_exn : (string * Column.packed) list -> t
val read_csv : string -> t
val copy : t -> t
val get_column : t -> string -> Column.packed option
val get_column_exn : t -> string -> Column.packed
val column_names : t -> string list
val named_columns : t -> (string * Column.packed) list
val to_string : ?headers_only:bool -> t -> string
val to_aligned_rows : t -> string list

(** [length t] returns the number of rows in t. *)
val length : t -> int

val num_rows : t -> int
val num_cols : t -> int

module Filter : sig
  type _ column

  (* TODO: avoid requiring [t] in the following. *)
  val int : t -> string -> int column
  val float : t -> string -> float column
  val string : t -> string -> string column

  type _ t
  val apply : 'a column -> 'b t -> ('a -> 'b) t
  val (@->) : 'a column -> 'b t -> ('a -> 'b) t
  val return : bool t
end

val filter : t -> 'a Filter.t -> 'a -> t
