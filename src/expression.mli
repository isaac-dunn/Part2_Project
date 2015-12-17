(* Expressions *)
(* Isaac Dunn 17/12/2015 *)

(** Some kind of type structure *)
structure Type : Type

(** Expressions with variables *)
type raw_expr

(** Expressions with de Bruijn indices *)
type expr

(** Gives human-readable representation *)
val string_of_expr : expr -> string

(** Function taking raw expressions to de Bruijn expressions *)
val convert_from_raw : raw_expr -> expr

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

