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

module Applicative : sig
  type nonrec 'a t = t -> index:int -> 'a
  module Open_on_rhs_intf : sig
    module type S = Applicative.S
  end
  include Applicative.S with type 'a t := 'a t
  include Applicative.Let_syntax with type 'a t := 'a t and module Open_on_rhs_intf := Open_on_rhs_intf

  val column : (module Array_intf.S with type t = 'a and type Elt.t = 'b) -> string -> 'b t
  val int : string -> int t
  val float : string -> float t
  val string : string -> string t
end

val filter : t -> bool Applicative.t -> t
