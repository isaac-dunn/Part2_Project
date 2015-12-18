(* Expressions *)
(* Isaac Dunn 17/12/2015 *)

(** Need some way of referring to locations for stores etc. *)
type loc = string

val string_of_loc : loc -> string

val random_loc : unit -> loc

type oper =
     Plus
   | Minus
   | Mult
   | Div
   | Mod
   | GT
   | Equals

(** Expressions with de Bruijn indices *)
type expr =
     Integer of int
   | Boolean of bool
   | Op of expr * oper * expr
   | If of expr * expr * expr
   | Assign of expr * expr
   | Deref of expr
   | Ref of expr
   | Loc of loc
   | Glo of loc
   | Skip
   | Seq of expr * expr
   | While of expr * expr
   | Var of int
   | Fn of Type.type_expr * expr
   | App of expr * expr
   | Let of Type.type_expr * expr * expr
   | Letrec of Type.type_expr * Type.type_expr * expr * expr
   | Cas of expr * expr * expr
   | Error of string

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

