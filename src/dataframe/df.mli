open Base

(* The phantom type is used to indicate whether a filter applies to the
   data or not. *)
type _ t

val create : (string * Column.packed) list -> [ `unfiltered ] t Or_error.t
val create_exn : (string * Column.packed) list -> [ `unfiltered ] t

(** [copy t] returns a new dataframe where all the columns from [t]
    have been copied. Note that this is not a deep-copy: column elements
    are shared which may have some consequences if they are mutable.
*)
val copy : _ t -> [`unfiltered ] t

(* The returned column below contains the filtered and unfiltered
   element. Maybe we should have separate functions for both cases.
*)
val get_column : _ t -> string -> Column.packed option
val get_column_exn : _ t -> string -> Column.packed
val add_column : [`unfiltered] t -> name:string -> (_, _) Column.t -> [ `unfiltered] t Or_error.t
val add_column_exn : [`unfiltered] t -> name:string -> (_, _) Column.t -> [ `unfiltered] t
val column_names : _ t -> string list
val column_types : _ t -> string list
val named_columns : _ t -> (string * Column.packed) list
val to_string : ?headers_only:bool -> _ t -> string
val to_aligned_rows : _ t -> string list

(** [length t] returns the number of rows in t. *)
val length : _ t -> int

val num_rows : _ t -> int
val num_cols : _ t -> int
val sort : _ -> [ `not_implemented_yet ]

module Csv : sig
  val read : string -> [ `not_implemented_yet ]
  val write : _ t -> string -> [ `not_implemented_yet ]
end

module Row_map : sig
  type nonrec 'a t
  include Applicative.S with type 'a t := 'a t
  include Applicative.Let_syntax with type 'a t := 'a t

  val column : ('a, 'b) Array_intf.t -> string -> 'a t
  val int : string -> int t
  val float : string -> float t
  val string : string -> string t
end

(** [filter t f] applies a filter to dataframe [t] and returns
    a new dataframe that share column data with [t].
*)
val filter : _ t -> bool Row_map.t -> [ `filtered ] t

(** [map t array_intf f] returns a column by applying [f] to rows in
    [t]. This creates a newly allocated column only containing the
    filtered elements from the initial dataframe.
*)
val map : _ t -> ('a, 'b) Array_intf.t -> 'a Row_map.t ->  ('a, 'b) Column.t

(** [map_and_column ?only_filtered t ~name f] returns a dataframe similar
    to [t] but also adding a column [name] which values are obtained by
    applying [f] to each row in [t].
*)
val map_and_add_column
 : [ `unfiltered] t -> name:string -> ('a, 'b) Array_intf.t -> 'a Row_map.t -> [ `unfiltered ] t Or_error.t

val map_and_add_column_exn
 : [ `unfiltered] t -> name:string -> ('a, 'b) Array_intf.t -> 'a Row_map.t -> [ `unfiltered ] t
