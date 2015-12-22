(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

open Pl_expression

module PLCorrectnessTest (Chk :
    Interfaces.Checker with module ProgImp = Program.PLProgram) = struct

    module C = Chk

    type case = (string array * C.ProgImp.ThrImp.StoreImp.store * bool)

    let test_cases = [
        (* Initial expression strings, global store, error possible *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], true);

        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 2)";
         |], [("x", Integer 2)], false);

        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 1, 1)";
         |], [("x", Integer 2)], false);

        ([| "if cas(Gx, 1, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], true);

        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Integer 5)], false);

        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Skip)], true);

        ([| "if cas(Gx, !Gx, 0) then skip else error(not cas)";
            "if cas(Gx, !Gx, 1) then skip else error(not cas)";
         |], [("x", Integer 2)], true);

        ([| "if cas(Gx, !Gx, 2) then skip else error(not cas)";
            "if cas(Gx, !Gx, 2) then skip else error(not cas)";
         |], [("x", Integer 2)], false);

        ([| "cas(Gx, !Gx, !Gx + 1)";
            "cas(Gx, !Gx, !Gx + 1)";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], true);

        (* Exclude one thread from entering *)
        ([| "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], false);

        (* Only change value if unchanged since read *)
        ([| "let val Vr : rf int = ref 0 in
             while if cas(Gx, !Vr, !Vr + 1) then false else true do
                Vr := !Gx done; cas(Gd0, false, true)";
            "let val Vr : rf int = ref 0 in
             while if cas(Gx, !Vr, !Vr + 2) then false else true do
                Vr := !Gx done; cas(Gd1, false, true)";
            "if !Gd0 then if !Gd1 then if !Gx = 103 then skip
                else error(race condition) else skip else skip";
         |], [("x", Integer 100); ("d0", Boolean false); ("d1", Boolean false)],
            false);

        (* Large state space; obvious result though *)
        ([| "while 2 > !Gx do if cas(Gx, !Gx, !Gx+1) then skip else skip done";
            "while 3 > !Gx do if cas(Gx, !Gx, !Gx+1) then skip else skip done";
        |], [("x", Integer 0)], false);

        (* Large state space *)
        ([| "let val Vr : rf int = ref 0 in
                while 4 > !Vr do if cas(Gx, !Vr, !Vr+1) then Vr := !Gx else Vr := !Gx done"; 
            "let val Vr : rf int = ref 0 in
                while 3 > !Vr do if cas(Gx, !Vr, !Vr+1) then Vr := !Gx else Vr := !Gx done"; 
            "let val Vr : rf int = ref 0 in
                while 3 > !Vr do if cas(Gx, !Vr, !Vr+1) then Vr := !Gx else Vr := !Gx done"; 
        |], [("x", Integer 0)], false);
    ]

    let run_test (es, g, err_poss) =
        C.max_depth := 0; C.calls := 0;
        let convert e = (Pl_parser.expr_of_string e, C.ProgImp.ThrImp.StoreImp.empty) in
        let tds = Array.map convert es in
        if C.check (tds, g) [] = err_poss then (* Failure *)
             (print_string "Below initial state expected ";
              (if err_poss then print_string "to have error but none found\n"
                else print_string "not to have error but error found\n");
              print_endline (C.ProgImp.string_of_program (tds, g));
              print_newline ();
              false)
        else (print_endline ("Calls: " ^ (string_of_int !C.calls) ^ "\n"
                            ^"Depth: " ^ (string_of_int !C.max_depth) ^ "\n");
              true) (* Success *)

    let all_tests_passed =
        let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
            conj (List.map run_test test_cases)

    let print_all_tests_passed () = print_endline ("Checker Correctness Tests All Passed: "
                            ^ (string_of_bool all_tests_passed))
end

module SPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SimplePLChecker)

let () = SPLCheckerCorrectnessTest.print_all_tests_passed()

