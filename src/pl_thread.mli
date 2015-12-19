(** Thread Interface *)

module ExImp : Expression
module StoreImp : Store with module ExprImp = ExImp

(** A thread step is a new expression, optional updates to the stores, and
* which global location, if any, was accessed *)
type step = { new_expr : ExImp.expr ;
              s_update : StoreImp.store_update option ;
              g_update : StoreImp.store_update option ;
              g_loc    : ExImp.loc option ;
            }

(* A transition is a new expression, store updates for local and global stores, and which
* global location was accessed *)
type transition = { next_expr : ExImp.expr ;
                    s_updates : StoreImp.store ;
                    g_updates : StoreImp.store ;
                    g_loc     : ExImp.loc ;
                  }

(** Given expression, local store, global store, gives next thread step if it exists *)
val next_step : ExImp.expr * StoreImp.store * StoreImp.store -> step option

(* Given expression, local state and global state, gives new expression
* local store update, global store update and global location touched *)
val next_transition : ExImp.expr * StoreImp.store * StoreImp.store -> transition option
