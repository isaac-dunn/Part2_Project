(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module C : Interfaces.Checker

val test_cases : (string array * Store.PLStore.store * bool) list

val run_test : (string array * Store.PLStore.store * bool) -> bool

val all_tests_passed : bool
