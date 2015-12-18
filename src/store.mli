(* Stores *)
(* Isaac Dunn 17/12/2015 *)

(** Maps locations to values *)
type store

type store_update = Expression.loc * Expression.expr

(** Human-readable representation *)
val string_of_store : store -> string

(** Given a store and location, returns value if it exists *)
val get : store -> Expression.loc -> Expression.expr option

(** Given a store and a new mapping gives old store updated with mapping *)
val update : store -> store_update -> store

(** Given a store and a store representing updates gives updated store *)
val extend : store -> store -> store

(** Given store, gives location that maps to nothing in it *)
val get_fresh_loc : store -> Expression.loc

