(* TODO: maybe add a phantom type to describe whether a dataframe is
   filtered or not.
*)
open Base

type t

val create : (string * Column.packed) list -> t Or_error.t
val create_exn : (string * Column.packed) list -> t

(** [copy t] returns a new dataframe where all the columns from [t]
    have been copied. Note that this is not a deep-copy: column elements
    are shared which may have some consequences if they are mutable.
*)
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
val sort : _ -> [ `not_implemented_yet ]

module Csv : sig
  val read : string -> [ `not_implemented_yet ]
  val write : t -> string -> [ `not_implemented_yet ]
end

module Row_map : sig
  type nonrec 'a t
  include Applicative.S with type 'a t := 'a t
  include Applicative.Let_syntax with type 'a t := 'a t

  val column : (module Array_intf.S with type t = 'a and type Elt.t = 'b) -> string -> 'b t
  val int : string -> int t
  val float : string -> float t
  val string : string -> string t
end

(** [filter t f] applies a filter to dataframe [t] and returns
    a new dataframe that share column data with [t].
*)
val filter : t -> bool Row_map.t -> t

(** [map t array_intf f] returns a column by applying [f] to rows in
    [t]. This creates a newly allocated column only containing the
    filtered elements from the initial dataframe.
*)
val map : t -> ('a, 'b) Array_intf.t -> 'a Row_map.t ->  ('a, 'b) Column.t

(** [map_and_column ?only_filtered t ~name f] returns a dataframe similar
    to [t] but also adding a column [name] which values are obtained by
    applying [f] to each row in [t].
*)
val map_and_add_column
 : ?only_filtered:bool -> t -> name:string -> 'a Row_map.t -> ('a, 'b) Array_intf.t -> t
