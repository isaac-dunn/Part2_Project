(* Expressions *)
(* Isaac Dunn 17/12/2015 *)

(** Expressions with de Bruijn indices *)
type expr

(** Need some way of referring to locations for stores etc. *)
type loc = string

(** Gives human-readable representation *)
val string_of_expr : expr -> string

(** Returns true iff expression represents error state *)
val is_error : expr -> bool

(** Returns true iff expression is a value *)
val is_value : expr -> bool

(** Substitutes first argument for outmost variable in second *)
val substitute_outmost : expr -> expr -> expr

(** Increments all indices at least the first argument in the second *)
val shift : int -> expr -> expr

(** Takes integer n and expression e, and swaps n and n+1 indices in e *)
val swap : int -> expr -> expr

