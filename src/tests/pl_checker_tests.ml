(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

open Pl_expression

module PLCorrectnessTest (Chk :
    Interfaces.Checker with module ProgImp = Program.PLProgram) = struct

    module C = Chk

    type case = (string array * C.ProgImp.ThrImp.StoreImp.store * bool)

    let test_cases =
        let rec array_store n = if n = 0 then [("0", Integer 0)]
            else  (string_of_int n,
                    Integer 0)::(array_store (n-1)) in
        let int_to_loc_expr_str = "fn Vx : int =>
            if Vx=0 then G0 else if Vx=1 then G1 else if Vx=2 then G2 else
            if Vx=3 then G3 else if Vx=4 then G4 else if Vx=5 then G5 else
            if Vx=6 then G6 else if Vx=7 then G7 else if Vx=8 then G8 else
            if Vx=9 then G9 else if Vx=10 then G10 else if Vx=11 then G11 else
            if Vx=12 then G12 else if Vx=13 then G13 else if Vx=14 then G14 else
            if Vx=15 then G15 else if Vx=16 then G16 else if Vx=17 then G17 else
            if Vx=18 then G18 else if Vx=19 then G19 else if Vx=20 then G20 else
            if Vx=21 then G21 else if Vx=22 then G22 else if Vx=23 then G23 else
            if Vx=24 then G24 else if Vx=25 then G25 else if Vx=26 then G26 else
            if Vx=27 then G27 else if Vx=28 then G28 else if Vx=29 then G29 else
            if Vx=30 then G30 else if Vx=31 then G31 else if Vx=32 then G32 else
            if Vx=33 then G33 else if Vx=34 then G34 else if Vx=35 then G35 else
            if Vx=36 then G36 else if Vx=37 then G37 else if Vx=38 then G38 else
            if Vx=39 then G39 else if Vx=40 then G40 else if Vx=41 then G41 else
            if Vx=42 then G42 else if Vx=43 then G43 else if Vx=44 then G44 else
            if Vx=45 then G45 else if Vx=46 then G46 else if Vx=47 then G47 else
            if Vx=48 then G48 else if Vx=49 then G49 else if Vx=50 then G50 else
            if Vx=51 then G51 else if Vx=52 then G52 else if Vx=53 then G53 else
            if Vx=54 then G54 else if Vx=55 then G55 else if Vx=56 then G56 else
            if Vx=57 then G57 else if Vx=58 then G58 else if Vx=59 then G59 else
            if Vx=60 then G60 else if Vx=61 then G61 else if Vx=62 then G62 else
            if Vx=63 then G63 else if Vx=64 then G64 else if Vx=65 then G65 else
            if Vx=66 then G66 else if Vx=67 then G67 else if Vx=68 then G68 else
            if Vx=69 then G69 else if Vx=70 then G70 else if Vx=71 then G71 else
            if Vx=72 then G72 else if Vx=73 then G73 else if Vx=74 then G74 else
            if Vx=75 then G75 else if Vx=76 then G76 else if Vx=77 then G77 else
            if Vx=78 then G78 else if Vx=79 then G79 else if Vx=80 then G80 else
            if Vx=81 then G81 else if Vx=82 then G82 else if Vx=83 then G83 else
            if Vx=84 then G84 else if Vx=85 then G85 else if Vx=86 then G86 else
            if Vx=87 then G87 else if Vx=88 then G88 else if Vx=89 then G89 else
            if Vx=90 then G90 else if Vx=91 then G91 else if Vx=92 then G92 else
            if Vx=93 then G93 else if Vx=94 then G94 else if Vx=95 then G95 else
            if Vx=96 then G96 else if Vx=97 then G97 else if Vx=98 then G98 else
            if Vx=99 then G99 else if Vx=100 then G100 else if Vx=101 then G101 else
            if Vx=102 then G102 else if Vx=103 then G103 else if Vx=104 then G104 else
            if Vx=105 then G105 else if Vx=106 then G106 else if Vx=107 then G107 else
            if Vx=108 then G108 else if Vx=109 then G109 else if Vx=110 then G110 else
            if Vx=111 then G111 else if Vx=112 then G112 else if Vx=113 then G113 else
            if Vx=114 then G114 else if Vx=115 then G115 else if Vx=116 then G116 else
            if Vx=117 then G117 else if Vx=118 then G118 else if Vx=119 then G119 else
            if Vx=120 then G120 else if Vx=121 then G121 else if Vx=122 then G122 else
            if Vx=123 then G123 else if Vx=124 then G124 else if Vx=125 then G125 else
            if Vx=126 then G126 else if Vx=127 then G127 else error(out of bounds)" in

    [
        (* Initial expression strings, global store, error possible *)
        ([| "cas(Gx, 2, 0)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], false);

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

        (* Large state space *)
        ([| "let val Vr : rf int = ref 0 in
                while 2 > !Vr do if cas(Gx, !Vr, !Vr+1) then Vr := !Gx else Vr := !Gx done"; 
            "let val Vr : rf int = ref 0 in
                while 2 > !Vr do if cas(Gx, !Vr, !Vr+1) then Vr := !Gx else Vr := !Gx done"; 
        |], [("x", Integer 0)], false);

        (* Indexer example *)
        ([| "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 0 in
             let val Vm : rf int = ref 0 in
             let val Vw : rf int = ref 0 in
             let val Vh : rf int = ref 0 in
             let val Vhash : int -> int = fn Vx : int => (Vx * 7) % Vsize in
             let val Vgetmsg : unit -> int = fn Vu : unit => !Vm * 11 + Vtid in
             while Vmax > !Vm do
                Vm := !Vm + 1;
                Vw := (Vgetmsg @ skip);
                Vh := (Vhash @ !Vw);
                while (if cas(!Gtable @ !Vh, 0, !Vw) then false else true) do
                    Vh := (!Vh + 1) % Vsize
                done
            done";

            "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 1 in
             let val Vm : rf int = ref 0 in
             let val Vw : rf int = ref 0 in
             let val Vh : rf int = ref 0 in
             let val Vhash : int -> int = fn Vx : int => (Vx * 7) % Vsize in
             let val Vgetmsg : unit -> int = fn Vu : unit => !Vm * 11 + Vtid in
             while Vmax > !Vm do
                Vm := !Vm + 1;
                Vw := (Vgetmsg @ skip);
                Vh := (Vhash @ !Vw);
                while (if cas(!Gtable @ !Vh, 0, !Vw) then false else true) do
                    Vh := (!Vh + 1) % Vsize
                done
            done";

        |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), false);

        (* Thread 0 accesses even array indices; thread 1 accesses odd *)
        ([| "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 0 in
             let val Vi : rf int = ref 0 in
             while Vmax > !Vi do
                if cas(!Gtable @ (Vtid + 2 * !Vi), 0, 1)
                then Vi := !Vi + 1
                else error(array element already reached)
             done";

            "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 1 in
             let val Vi : rf int = ref 0 in
             while Vmax > !Vi do
                if cas(!Gtable @ (Vtid + 2 * !Vi), 0, 1)
                then Vi := !Vi + 1
                else error(array element already reached)
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), false);

        (* Thread 0 accesses 0,3,6,9; thread 1 accesses 1,5,9; error at 9 *)
        ([| "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 0 in
             let val Vi : rf int = ref 0 in
             while Vmax > !Vi do
                if cas(!Gtable @ (Vtid + 3 * !Vi), 0, 1)
                then Vi := !Vi + 1
                else error(array element already reached)
             done";

            "let val Vsize : int = 128 in
             let val Vmax : int = 4 in
             let val Vtid : int = 1 in
             let val Vi : rf int = ref 0 in
             while Vmax > !Vi do
                if cas(!Gtable @ (Vtid + 4 * !Vi), 0, 1)
                then Vi := !Vi + 1
                else Vi := !Vi + 1
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), true);

        (* first set array values in thread 0, then check them in thread 1 *)
        ([| "let val Vsize : int = 3 in
             let val Vi : rf int = ref 0 in
             while Vsize > !Vi do
                if cas(!Gtable @ !Vi, 0, (!Vi % 4) + 1)
                then Vi := !Vi + 1
                else error(array should be initially zero)
             done; if cas(Gready, false, true) then skip
                    else error(should only be ready now not before)";

            "let val Vlimit : int = 8 in
             let val Vcounter : rf int = ref 0 in (
             while if !Gready then false else Vlimit > !Vcounter
                do Vcounter := !Vcounter + 1 done;
             let val Vsize : int = 3 in
             let val Vi : rf int = ref 0 in
             while Vsize > !Vi do
                if cas(!Gtable @ !Vi, (!Vi % 4) + 1, 0)
                then Vi := !Vi + 1
                else if Vlimit > !Vcounter then error(wrong value in table)
                     else Vi := !Vi + 1
            done)";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
               (("ready", Boolean false)::(array_store 8)), false);

        (* Producer/Consumer *)
        ([| "let val Vsize : int = 8 in
             let val Vi : rf int = ref 0 in
             let val Vlimit : int = 2 in
             let val Vctr : rf int = ref 0 in
             let val Vhash : int -> int =
                fn Vx : int => ((((12773*Vx)%179)*7)%100)+1 in
             while Vlimit > !Vctr do
                if !(!Gtable @ !Vi) = 0
                then if cas(!Gtable @ (!Vi+1), 0, Vhash @ (!Vi+1))
                     then if cas(!Gtable @ !Vi, 0, 1)
                        then Vctr := !Vctr + 1; Vi := (!Vi + 2) % Vsize
                        else error(marker should still be zero)
                     else error(supposed to be empty slot)
                else Vctr := !Vctr + 1
             done";

            "let val Vsize : int = 8 in
             let val Vi : rf int = ref 0 in
             let val Vlimit : int = 2 in
             let val Vctr : rf int = ref 0 in
             while Vlimit > !Vctr do
                if !(!Gtable @ !Vi) = 1
                then let val Vn : int = !(!Gtable @ (!Vi+1)) in
                   if Vn = 0 then error(supposed to be full slot)
                   else if cas(!Gtable @ (!Vi+1), Vn, 0)
                        then if cas(!Gtable @ !Vi, 1, 0)
                             then Vctr := !Vctr + 1; Vi := (!Vi + 2) % Vsize
                             else error(marker should still be one)
                        else error(value changed since read)
                else Vctr := !Vctr + 1
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
               (array_store 8), false);
    ]

    let run_test (es, g, err_poss) =
        C.max_depth := 0; C.calls := 0;
        let convert e = (Pl_parser.expr_of_string e, C.ProgImp.ThrImp.StoreImp.empty) in
        let tds = Array.map convert es in
        if C.error_free (tds, g) = err_poss then (* Failure *)
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
                            ^ (string_of_bool all_tests_passed) ^ "\n\n")
end

module SPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SimplePLChecker)

let () = SPLCheckerCorrectnessTest.print_all_tests_passed()

module DPORPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.DPORPLChecker)

let () = DPORPLCheckerCorrectnessTest.print_all_tests_passed()

