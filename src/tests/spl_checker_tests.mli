(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module C : Interfaces.Checker

val test_cases : (Pl_expression.expr array * Store.PLStore.store * bool) list

val run_test : (Pl_expression.expr list * Store.PLStore.store * bool) -> bool

val all_tests_passed : bool
