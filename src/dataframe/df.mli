(* TODO: add [cast].
   TODO: maybe add [join] ?
*)
open Base

(** The parameter type is used to indicate whether a filter applies to
    the data or not.
    Functions that return an [`unfiltered] usually make a copy of the whole
    dataframe.
*)
type _ t

(** {3 Basic Functions} *)

(** [length t] returns the number of rows in t. *)
val length : _ t -> int

(** [num_rows t] returns the number of rows in t. *)
val num_rows : _ t -> int

(** [num_cols t] returns the number of columns in t. *)
val num_cols : _ t -> int


(** {3 Dataframe Creation} *)

(** [create named_columns] returns a new dataframe based on the columns
    from [named_columns].
    This returns an error if some of the columns have different sizes or
    if the same column name appears multiple times.
*)
val create : (string * Column.packed) list -> [ `unfiltered ] t Or_error.t

(** [create_exn] is similar to [create] but raises an exception on errors.
*)
val create_exn : (string * Column.packed) list -> [ `unfiltered ] t

(** [copy t] returns a new dataframe where all the columns from [t]
    have been copied. Note that this is not a deep-copy: column elements
    are shared which may have some consequences if they are mutable.
*)
val copy : _ t -> [`unfiltered ] t

(** {3 Column Operations} *)

(** [get_column t column_name] returns the column of [t] which name matches
    [column_name]. If no such column exist an error is returned.
*)
val get_column : [ `unfiltered ] t -> string -> Column.packed option

(** [get_column_exn] is similar to [get_column] but raises an expection on
    errors.
*)
val get_column_exn : [ `unfiltered ] t -> string -> Column.packed

(** [add_column t n c] adds a new column [c] with name [n] to dataframe [t].
    An error is returned if there is already a column with that name in [t]
    or if column [c]'s length does not match the dataframe length.
*)
val add_column : [`unfiltered] t -> name:string -> (_, _) Column.t -> [ `unfiltered] t Or_error.t

(** [add_column_exn] is similar to [add_column] but raises on errors. *)
val add_column_exn : [`unfiltered] t -> name:string -> (_, _) Column.t -> [ `unfiltered] t

(** [column_names t] returns the list of names of columns appearing in [t].
*)
val column_names : _ t -> string list

(** [column_types t] returns the list of types (as strings) of columns
    appearing in [t].
*)
val column_types : _ t -> string list

(** [named_columns t] returns all the columns from [t] together with their
    names.
*)
val named_columns : _ t -> (string * Column.packed) list

(** [filter_columns t ~names] returns a dataframe only containing
    columns from [names]. If there are column names in [names] that do
    not exist in [t], an error is returned.
*)
val filter_columns : 'a t -> names:string list -> 'a t Or_error.t

(** Similar to [filter_columns] but raises an exception rather than
    returning an error.
*)
val filter_columns_exn : 'a t -> names:string list -> 'a t

(** {3 Pretty Printing } *)
val to_string : ?headers_only:bool -> _ t -> string
val to_aligned_rows : _ t -> string list
val print : ?out_channel:Stdio.Out_channel.t -> _ t -> unit

(** {3 Mapping and Filtering } *)

(** The [R] module contains an applicative used for maps from rows to
    values. These can be used with the [filter] and [map] functions
    below.
    For example, filtering all rows where column "col" has a value 42
    and column "col'" has value 3.14 can be done via the following (after
    opening [R.Let_syntax]):
    {[
      Df.filter df
        [%map_open
          let c1 = Df.R.int "col"
          and c2 = Df.R.int "col'" in
          c1 = 42 && c2 =. 3.14]
    |}
*)
module R : sig
  type nonrec 'a t
  include Applicative.S with type 'a t := 'a t
  include Applicative.Let_syntax with type 'a t := 'a t

  (** [column array_intf column_name] extracts the values from
      the column named [column_name] if it matches [array_intf].
  *)
  val column : ('a, 'b) Array_intf.t -> string -> 'a t
  val int : string -> int t
  val float : string -> float t
  val string : string -> string t
end

(** [filter t f] applies a filter to dataframe [t] and returns
    a new dataframe that share column data with [t].
*)
val filter : _ t -> bool R.t -> [ `filtered ] t

(** [map t array_intf f] returns a column by applying [f] to rows in
    [t]. This creates a newly allocated column only containing the
    filtered elements from the initial dataframe.
*)
val map : _ t -> ('a, 'b) Array_intf.t -> 'a R.t ->  ('a, 'b) Column.t

(** [map_and_column ?only_filtered t ~name f] returns a dataframe similar
    to [t] but also adding a column [name] which values are obtained by
    applying [f] to each row in [t].
*)
val map_and_add_column
 : [ `unfiltered] t -> name:string -> ('a, 'b) Array_intf.t -> 'a R.t -> [ `unfiltered ] t Or_error.t

val map_and_add_column_exn
 : [ `unfiltered] t -> name:string -> ('a, 'b) Array_intf.t -> 'a R.t -> [ `unfiltered ] t

(** [fold t ~init ~f] folds over filtered rows of [t] in order.
*)
val fold : _ t -> init:'a -> f:('a -> 'a) R.t -> 'a

val reduce : _ t -> 'a R.t -> f:('a -> 'a -> 'a) -> 'a option

(** {3 Sorting and Grouping } *)

(** [sort t r ~compare] returns a new dataframe by sorting [t] based on
    the [compare] function applied to the result of [r] on each row.
*)
val sort : _ t -> 'a R.t -> compare:('a -> 'a -> int) -> [ `unfiltered ] t

(** [sort_by ?reverse t ~name] returns a new dataframe by sorting [t] using the
    given column name.
    The default value for [reverse] is [false], if set the dataframe is returned
    in reversed order.
*)
val sort_by : ?reverse:bool -> _ t -> name:string -> [ `unfiltered ] t

(** [group t f] returns a list of dataframe containing an element for each
    value obtained by applying [f] to each row. This list is made of pairs
    where the first element is the output of [f] and the second the initial
    dataframe filtered to only contain row which [f] output this value.

    The current implementation uses a polymorphic hashtbl so may have issues
    with complex or mutable types.
*)
val group
  :  _ t
  -> 'a R.t
  -> ('a * [ `filtered ] t) list

(** {3 Handling columns with specific types } *)

module Float : sig
  val sum : _ t -> name:string -> float
  val mean : _ t -> name:string -> float option
  val min : _ t -> name:string -> float option
  val max : _ t -> name:string -> float option
end

module Int : sig
  val sum : _ t -> name:string -> int
  val mean : _ t -> name:string -> float option
  val min : _ t -> name:string -> int option
  val max : _ t -> name:string -> int option
end

(** {3 Misc } *)

val filter_ : 'a t -> 'a Filter.t
