(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

module C = Checker.SimplePLChecker
open Pl_expression

let test_cases = [
    (* Initial expressions, global store, error possible *)
    (* thread 1, thread 2, global store, error possible *)
    ([|If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS");
     Cas(Glo "x", Integer 2, Integer 1)|], [("x", Integer 2)], true);

    ([|If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS");
     Cas(Glo "x", Integer 2, Integer 2)|], [("x", Integer 2)], false);

    ([|If (Cas (Glo "x", Integer 2, Integer 0), Integer 100, Error "Negative CAS");
     Cas(Glo "x", Integer 1, Integer 1)|], [("x", Integer 2)], false);

    ([|If (Cas (Glo "x", Integer 1, Integer 0), Integer 100, Error "Negative CAS");
     Cas(Glo "x", Integer 2, Integer 1)|], [("x", Integer 2)], true);

    ([|While (Cas (Glo "x", Boolean false, Skip), Error "Unreachable");
     While (Cas (Glo "x", Skip, Boolean true), Error "Unreachable")|],
     [("x", Integer 5)], false);

    ([|While (Cas (Glo "x", Boolean false, Skip), Error "Unreachable");
     While (Cas (Glo "x", Skip, Boolean true), Error "Unreachable")|],
     [("x", Skip)], true);

    ([|If (Cas (Glo "x", Deref (Glo "x"), Integer 0), Skip, Error "Cas failed");
     If (Cas (Glo "x", Deref (Glo "x"), Integer 1), Skip, Error "Cas failed")|],
     [("x", Integer 2)], true);

    ([|If (Cas (Glo "x", Deref (Glo "x"), Integer 2), Skip, Error "Cas failed");
     If (Cas (Glo "x", Deref (Glo "x"), Integer 2), Skip, Error "Cas failed")|],
     [("x", Integer 2)], false);
]

let run_test (es, g, err_poss) =
    let add_empty_store e = (e, C.ProgImp.ThrImp.StoreImp.empty) in
    let tds = Array.map add_empty_store es in
    if C.check (tds, g) [] = err_poss then true (* Success *)
    else (print_string "Below initial state expected ";
          (if err_poss then print_string " to have error but none found\n"
            else print_string " not to have error but error found\n");
          print_string (C.ProgImp.string_of_program (tds, g));
          print_newline ();
          false) (* Failure *)

let all_tests_passed =
    let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
        conj (List.map run_test test_cases)

let () = print_string ("Simple PL Checker Tests All Passed: "
                        ^ (string_of_bool all_tests_passed));
         print_newline ()

