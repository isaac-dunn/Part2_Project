(* Stores *)
(* Isaac Dunn 17/12/2015 *)

(** Maps locations to values *)
type store

(** Human-readable representation *)
val string_of_store : store -> string

(** Given a store and location, returns value if it exists *)
val get : store -> loc -> Expression.expr option

(** Given a store and a new mapping gives old store updated with mapping *)
val update : store -> loc -> Expression.expr -> store

(** Given store, gives location that maps to nothing in it *)
val get_fresh_loc : store -> loc

