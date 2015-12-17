(* Threads *)
(* Isaac Dunn 17/12/2015 *)

(** Some implementation of expressions *)
structure Expr : Expression

(** Some implementation of stores *)
structure Store : Store

(** A thread step is a new expression, optional updates to the stores, and
* which global location, if any, was accessed *)
type thread_step = { new_expr : expr ;
                     s_update : store_update option ;
                     g_update : store_update option ;
                     g_loc    : loc option ;
                   }

(* A transition is a new expression, store updates for local and global stores, and which
* global location was accessed *)
type thread_transition = { new_expr : expr ;
                           s_update : store ;
                           g_update : store ;
                           g_loc    : loc ;
                         }

(** Given expression, local store, global store, gives next thread step if it exists *)
val next_step : Expr.expr * Store.store * Store.store -> thread_step option

(* Given expression, local state and global state, gives new expression
* local store update, global store update and global location touched *)
val next_transition : Expr.expr * Store.store * Store.store -> thread_transition
