(* Unit tests for next transition function *)

module Thr = Pl_thread

(* ((e_str, s, g), next_transition) pairs *)
val test_cases : ((string * Thr.StoreImp.store * Thr.StoreImp.store) * Thr.transition option) list

val run_test : ((string * Thr.StoreImp.store * Thr.StoreImp.store) * Thr.transition option) -> bool

val all_tests_passed : bool

