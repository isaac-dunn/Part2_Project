(** PL Thread Interface *)

(** The store specifies not only the store but the expressions *)
module ExpImp : (Interfaces.Expression with type expr = Pl_expression.expr and type loc = Pl_expression.loc)
module StoreImp : (Interfaces.Store
    with type ExprImp.expr = ExpImp.expr
    and type ExprImp.loc = ExpImp.loc
    and type store = Store.PLStore.store)

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
    gives (next thread step, enabled bool) if it exists *)
val next_step : ExpImp.expr * StoreImp.store * StoreImp.store -> (step * bool) option

(* Given expression, local state and global state, gives new expression
* local store update, global store update global location touched, and enabled bool *)
val next_transition : ExpImp.expr * StoreImp.store * StoreImp.store -> (transition * bool) option


(* Given expression, local store, global store, returns pair of bools.
    First is true iff an error is reached.
    Second is true iff it stops at a non-value *)
val check_local : ExpImp.expr * StoreImp.store * StoreImp.store -> (bool * bool)
