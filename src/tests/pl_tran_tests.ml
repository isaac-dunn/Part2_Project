(* Unit tests for next transition function *)

module Thr = Pl_thread

open Pl_expression
open Type

(* ((e, s, g), next_transition) pairs *)
let test_cases = [
   
    ((If (Boolean true, Deref (Glo "x"), Skip), [], [("x", Integer 2)]),
    Some {Thr.next_expr = Integer 2;
     Thr.s_updates = []; Thr.g_updates = [];
     Thr.g_loc = "x"});

]

let run_test ((e, s, g), next_t) =
    Thr.next_transition (e, s, g) = next_t

let all_tests_passed =
    let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
        conj (List.map run_test test_cases)

let () = print_string ("PL Thread Next Transition Tests All Passed: "
                        ^ (string_of_bool all_tests_passed))

