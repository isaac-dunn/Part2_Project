(* Programs *)
(* Isaac Dunn 17/12/2015 *)

(** A program consists of threads and a global store *)
type state = (Expression.expr * Store.store) array * Store.store;;

(** A thread step with the index of the thread *)
type step = int * Thread.step
(** A thread transition with the index of the thread *)
type transition = int * Thread.transition

(** Gives a string representation of a program *)
val string_of_program : state -> string

(* Given a program step and a program state, gives the next program state *)
val apply_step : state -> step -> state

(* Given a program transition and a program state, gives the next program state *)
val apply_transition : state -> transition -> state

