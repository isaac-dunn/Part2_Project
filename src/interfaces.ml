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

    (** Returns global locations accessible *)
    val locations_accessed : expr -> loc list

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

    (** Gives minimal equivalent store *)
    val minimise : store -> store

    (** Given store, gives location that maps to nothing in it *)
    val get_fresh_loc : store -> ExprImp.loc

    (** Given store, gives all global locations in all stored expressions *)
    val globals_stored : store -> ExprImp.loc list

    (** Decides if two stores are equal *)
    val eq : store -> store -> bool

    (** Gives hash value for the store. Equal to those of equivalent stores *)
    val hash : store -> int
end

(** Thread Interface *)
module type Thread = sig
    module ExpImp : Expression
    module StoreImp : (Store with
        type ExprImp.expr = ExpImp.expr and
        type ExprImp.loc = ExpImp.loc)

    (** A thread step is a new expression, optional updates to the stores, and
    * which global location, if any, was accessed *)
    type step = { new_expr : ExpImp.expr ;
                  s_update : StoreImp.store_update option ;
                  g_update : StoreImp.store_update option ;
                  g_loc    : ExpImp.loc option ;
                }

    (* A transition is a new expression, store updates for local and global stores, and which
    * global location was accessed *)
    type transition = { next_expr : ExpImp.expr ;
                        s_updates : StoreImp.store ;
                        g_updates : StoreImp.store ;
                        g_loc     : ExpImp.loc ;
                      }

    val string_of_transition : transition -> string

    (** Given expression, local store, global store,
        gives next thread step and enabled, if it exists *)
    val next_step : ExpImp.expr * StoreImp.store * StoreImp.store -> (step * bool) option

    (* Given expression, local state and global state, gives new expression
    * local store update, global store update and global location touched *)
    val next_transition : ExpImp.expr * StoreImp.store * StoreImp.store -> (transition * bool) option

    (* Given expression, local store, global store, returns pair of bools.
        First is true iff an error is reached.
        Second is true iff it stops at a non-value *)
    val check_local : ExpImp.expr * StoreImp.store * StoreImp.store -> (bool * bool)
end

(* Program Interface *)
module type Program = sig
    module ThrImp : Thread

    (** A program consists of threads and a global store *)
    type state = (ThrImp.ExpImp.expr * ThrImp.StoreImp.store) array * ThrImp.StoreImp.store;;

    (** A thread step with the index of the thread *)
    type step = int * ThrImp.step
    (** A thread transition with the index of the thread *)
    type transition = int * ThrImp.transition

    (** Gives a string representation of a program *)
    val string_of_program : state -> string

    (* Given a program step and a program state, gives the next program state *)
    val apply_step : state -> step -> state

    (* Given a program transition and a program state, gives the next program state *)
    val apply_transition : state -> transition -> state

    (* Get edges in t_seq indices happens-before Hasse diagram *)
    val get_hasse_trace : transition list -> (int * int) list

    (* Given filename, t_seq, index Hasse diagram, write graphviz file *)
    val output_hasse_image : string -> transition list -> unit

    (* Type for making a Hashtbl implementation from *)
    type t = state
    (** Equality on program states *)
    val equal : state -> state -> bool
    (** Hashing function for states *)
    val hash : state -> int
end

(** Checker Interface *)
module type Checker = sig
    module ProgImp : Program

    val max_depth : int ref
    val calls : int ref

    (** True iff no error state is reachable *)
    val error_and_deadlock_free : ProgImp.state -> bool * bool
end

(* Test Module Interface *)
module type Test = sig
    module C : Checker

    (** Type of test case *)
    type case

    (** List of test cases *)
    val test_cases : case list

    (** True iff test passed *)
    val run_test : case -> bool

    val all_tests_passed : unit -> bool
end

