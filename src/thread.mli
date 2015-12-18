(* Threads *)
(* Isaac Dunn 17/12/2015 *)

(** A thread step is a new expression, optional updates to the stores, and
* which global location, if any, was accessed *)
type step = { new_expr : Expression.expr ;
              s_update : Store.store_update option ;
              g_update : Store.store_update option ;
              g_loc    : Expression.loc option ;
            }

(* A transition is a new expression, store updates for local and global stores, and which
* global location was accessed *)
type transition = { next_expr : Expression.expr ;
                    s_updates : Store.store ;
                    g_updates : Store.store ;
                    g_loc     : Expression.loc ;
                  }

(** Given expression, local store, global store, gives next thread step if it exists *)
val next_step : Expression.expr * Store.store * Store.store -> step option

(* Given expression, local state and global state, gives new expression
* local store update, global store update and global location touched *)
val next_transition : Expression.expr * Store.store * Store.store -> transition option
