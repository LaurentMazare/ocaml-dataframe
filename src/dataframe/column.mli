(** [('elt, 'storage) t] represents a column which elements have typed
    ['elt] and the whole data is stored using type ['storage].
*)
type ('elt, 'storage) t

(** {3 Column Creation} *)

(** [create array_intf elt ~len] creates a new column of length [len]
    using the storage described in [Array_intf]. All the elements of
    the column are initially set to [elt].
*)
val create : ('a, 'b) Array_intf.t -> 'a -> len:int -> ('a, 'b) t

(** [of_data array_intf data] returns a new column using [data] as
    storage.
*)
val of_data : ('a, 'b) Array_intf.t -> 'b -> ('a, 'b) t

(** [of_array array_intf array] creates a new column using data from
    [array]. This column is backed by a storage defined in [array_intf].
*)
val of_array : ('a, 'b) Array_intf.t -> 'a array -> ('a, 'b) t

(** [copy ?filter t] returns a new column obtained by copying [t].
    If [filter] is provided only elements that match [filter] are
    copied resulting in a column with potentially less elements
    than [t].
*)
val copy : ?filter:Bool_array.t -> ('a, 'b) t -> ('a, 'b) t

(** [select t ~indexes] returns a new column using the same storage as
    [t]. The elements of this new column are obtained by taking values
    from [t] at the indexes specified in [indexes]. The same index can
    be specified multiple times in [indexes].
*)
val select : ('a, 'b) t -> indexes:int array -> ('a, 'b) t

(** {3 Accessors} *)

val mod_ : ('a, 'b) t -> ('a, 'b) Array_intf.t

(** [get t index] returns the element of [t] stored at the specified [index].
    If [index] is negative or greater than or equal to the number of elements
    in [t] an error is raised.
*)
val get : ('a, _) t -> int -> 'a

(** [set t index value] sets the element of [t] at index [index] to the
    specified value.
*)
val set : ('a, _) t -> int -> 'a -> unit

(** [length t] returns the number of elements stored in column [t]. *)
val length : _ t -> int

(** [elt_name t] returns the kind of elements stored in [t] as a string. *)
val elt_name : _ t -> string

(** {3 Pretty Printing} *)

(** [get_string t index] returns a pretty-print of the element at the
    specified [index] in [t].
*)
val get_string : _ t -> int -> string

(** [to_string t] returns a multi-line version of elements stored in [t].
    The number of printed elements can be specified via [max_rows] and
    an optional filter can be applied.
*)
val to_string : ?max_rows:int -> ?filter:Bool_array.t -> _ t -> string

val fold : ('a, _) t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val min : ('a, _) t -> 'a option
val max : ('a, _) t -> 'a option

(** {3 Operating on Packed Values} *)

(** The [packed] type is used to abstract the type of elements and
    the storage used by a column.
*)
type packed = P : _ t -> packed

(** {4 Extracting Type Columns} *)

val extract : packed -> ('a, 'b) Array_intf.t -> ('a, 'b) t option
val extract_exn : packed -> ('a, 'b) Array_intf.t -> ('a, 'b) t

(** {4 Other Operations} *)

val packed_copy : ?filter:Bool_array.t -> packed -> packed
val packed_length : packed -> int
val packed_elt_name : packed -> string
val packed_to_string : ?max_rows:int -> ?filter:Bool_array.t -> packed -> string
val packed_get_string : packed -> int -> string
val packed_select : packed -> indexes:int array -> packed
