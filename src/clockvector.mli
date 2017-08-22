type cv

val fresh : int -> cv

val size : cv -> int

val get : cv -> int -> int

val set : cv -> int -> int -> unit

val string_of_cv : cv -> string

exception MismatchedSizes

val max : cv -> cv -> cv

