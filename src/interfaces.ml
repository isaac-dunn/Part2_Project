(** Expression Interface *)
module type Expression = sig
    (** Need some way of referring to locations for stores etc. *)
    type loc

    (** Gives human-readable representation *)
    val string_of_loc : loc -> string

    (** Gives random location *)
    val random_loc : unit -> loc

    (** Expressions with de Bruijn indices *)
    type expr

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
end



(** Store Interface *)
module type Store = sig 
    module ExprImp : Expression

    (** Maps locations to values *)
    type store

    type store_update = ExprImp.loc * ExprImp.expr

    (** Human-readable representation *)
    val string_of_store : store -> string

    (** Empty store, defined for no locations *)
    val empty : store

    (** Given a store and location, returns value if it exists *)
    val get : store -> ExprImp.loc -> ExprImp.expr option

    (** Given a store and a new mapping gives old store updated with mapping *)
    val update : store -> store_update -> store

    (** Given a store and a store representing updates gives updated store *)
    val extend : store -> store -> store

    (** Given store, gives location that maps to nothing in it *)
    val get_fresh_loc : store -> ExprImp.loc
end

