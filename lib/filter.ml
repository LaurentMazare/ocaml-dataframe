type _ t =
  | No_filter : int -> [ `unfiltered ] t (* Stores the length *)
  | Filter : Bool_array.t -> [ `filtered ] t
