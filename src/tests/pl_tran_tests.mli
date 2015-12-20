(* Unit tests for next transition function *)

module Thr = Pl_thread

(* ((e, s, g), next_transition) pairs *)
val test_cases : ((Thr.ExpImp.expr * Thr.StoreImp.store * Thr.StoreImp.store) * Thr.transition option) list

val run_test : ((Thr.ExpImp.expr * Thr.StoreImp.store * Thr.StoreImp.store) * Thr.transition option) -> bool

val all_tests_passed : bool

