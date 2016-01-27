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
        let int_to_loc_expr_str = "fn x =>
            if x=0 then G0 else if x=1 then G1 else if x=2 then G2 else
            if x=3 then G3 else if x=4 then G4 else if x=5 then G5 else
            if x=6 then G6 else if x=7 then G7 else if x=8 then G8 else
            if x=9 then G9 else if x=10 then G10 else if x=11 then G11 else
            if x=12 then G12 else if x=13 then G13 else if x=14 then G14 else
            if x=15 then G15 else if x=16 then G16 else if x=17 then G17 else
            if x=18 then G18 else if x=19 then G19 else if x=20 then G20 else
            if x=21 then G21 else if x=22 then G22 else if x=23 then G23 else
            if x=24 then G24 else if x=25 then G25 else if x=26 then G26 else
            if x=27 then G27 else if x=28 then G28 else if x=29 then G29 else
            if x=30 then G30 else if x=31 then G31 else if x=32 then G32 else
            if x=33 then G33 else if x=34 then G34 else if x=35 then G35 else
            if x=36 then G36 else if x=37 then G37 else if x=38 then G38 else
            if x=39 then G39 else if x=40 then G40 else if x=41 then G41 else
            if x=42 then G42 else if x=43 then G43 else if x=44 then G44 else
            if x=45 then G45 else if x=46 then G46 else if x=47 then G47 else
            if x=48 then G48 else if x=49 then G49 else if x=50 then G50 else
            if x=51 then G51 else if x=52 then G52 else if x=53 then G53 else
            if x=54 then G54 else if x=55 then G55 else if x=56 then G56 else
            if x=57 then G57 else if x=58 then G58 else if x=59 then G59 else
            if x=60 then G60 else if x=61 then G61 else if x=62 then G62 else
            if x=63 then G63 else if x=64 then G64 else if x=65 then G65 else
            if x=66 then G66 else if x=67 then G67 else if x=68 then G68 else
            if x=69 then G69 else if x=70 then G70 else if x=71 then G71 else
            if x=72 then G72 else if x=73 then G73 else if x=74 then G74 else
            if x=75 then G75 else if x=76 then G76 else if x=77 then G77 else
            if x=78 then G78 else if x=79 then G79 else if x=80 then G80 else
            if x=81 then G81 else if x=82 then G82 else if x=83 then G83 else
            if x=84 then G84 else if x=85 then G85 else if x=86 then G86 else
            if x=87 then G87 else if x=88 then G88 else if x=89 then G89 else
            if x=90 then G90 else if x=91 then G91 else if x=92 then G92 else
            if x=93 then G93 else if x=94 then G94 else if x=95 then G95 else
            if x=96 then G96 else if x=97 then G97 else if x=98 then G98 else
            if x=99 then G99 else if x=100 then G100 else if x=101 then G101 else
            if x=102 then G102 else if x=103 then G103 else if x=104 then G104 else
            if x=105 then G105 else if x=106 then G106 else if x=107 then G107 else
            if x=108 then G108 else if x=109 then G109 else if x=110 then G110 else
            if x=111 then G111 else if x=112 then G112 else if x=113 then G113 else
            if x=114 then G114 else if x=115 then G115 else if x=116 then G116 else
            if x=117 then G117 else if x=118 then G118 else if x=119 then G119 else
            if x=120 then G120 else if x=121 then G121 else if x=122 then G122 else
            if x=123 then G123 else if x=124 then G124 else if x=125 then G125 else
            if x=126 then G126 else if x=127 then G127 else error(out of bounds)" in

    [
        (* Test 0 *)
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

        (* Test 8 *)
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
        ([| "let r = ref 0 in
             while if cas(Gx, !r, !r + 1) then false else true do
                r := !Gx done; cas(Gd0, false, true)";
            "let r = ref 0 in
             while if cas(Gx, !r, !r + 2) then false else true do
                r := !Gx done; cas(Gd1, false, true)";
            "if !Gd0 then if !Gd1 then if !Gx = 103 then skip
                else error(race condition) else skip else skip";
         |], [("x", Integer 100); ("d0", Boolean false); ("d1", Boolean false)],
            false);

        (* Test 12 *)
        (* Large state space *)
        ([| "let r = ref 0 in
                while 2 > !r do if cas(Gx, !r, !r+1) then r := !Gx else r := !Gx done"; 
            "let r= ref 0 in
                while 2 > !r do if cas(Gx, !r, !r+1) then r := !Gx else r := !Gx done"; 
        |], [("x", Integer 0)], false);

        (* Indexer example *)
        ([| "let size= 128 in
             let max = 4 in
             let tid = 0 in
             let m = ref 0 in
             let w = ref 0 in
             let h = ref 0 in
             let hash = fn x => (x * 7) % size in
             let getmsg = fn u => !m * 11 + tid in
             while max > !m do
                m := !m + 1;
                w := (getmsg @ skip);
                h := (hash @ !w);
                while (if cas(!Gtable @ !h, 0, !w) then false else true) do
                    h := (!h + 1) % size
                done
            done";

            "let size = 128 in
             let max = 4 in
             let tid = 1 in
             let m = ref 0 in
             let w = ref 0 in
             let h = ref 0 in
             let hash = fn x => (x * 7) % size in
             let getmsg = fn u => !m * 11 + tid in
             while max > !m do
                m := !m + 1;
                w := (getmsg @ skip);
                h := (hash @ !w);
                while (if cas(!Gtable @ !h, 0, !w) then false else true) do
                    h := (!h + 1) % size
                done
            done";

        |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), false);

        (* Test 14 *)
        (* Thread 0 accesses even array indices; thread 1 accesses odd *)
        ([| "let size = 128 in
             let max = 4 in
             let tid = 0 in
             let i = ref 0 in
             while max > !i do
                if cas(!Gtable @ (tid + 2 * !i), 0, 1)
                then i := !i + 1
                else error(array element already reached)
             done";

            "let size = 128 in
             let max = 4 in
             let tid = 1 in
             let i = ref 0 in
             while max > !i do
                if cas(!Gtable @ (tid + 2 * !i), 0, 1)
                then i := !i + 1
                else error(array element already reached)
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), false);

        (* Test 15 *)
        (* Thread 0 accesses 0,3,6,9; thread 1 accesses 1,5,9; error at 9 *)
        ([| "let size = 128 in
             let max = 4 in
             let tid = 0 in
             let i = ref 0 in
             while max > !i do
                if cas(!Gtable @ (tid + 3 * !i), 0, 1)
                then i := !i + 1
                else error(array element already reached)
             done";

            "let size = 128 in
             let max = 4 in
             let tid = 1 in
             let i = ref 0 in
             while max > !i do
                if cas(!Gtable @ (tid + 4 * !i), 0, 1)
                then i := !i + 1
                else i := !i + 1
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
                (array_store 128), true);

        (* Test 16 *)
        (* first set array values in thread 0, then check them in thread 1 *)
        ([| "let size = 3 in
             let i = ref 0 in
             while size > !i do
                if cas(!Gtable @ !i, 0, (!i % 4) + 1)
                then i := !i + 1
                else error(array should be initially zero)
             done; if cas(Gready, false, true) then skip
                    else error(should only be ready now not before)";

            "let limit = 8 in
             let counter = ref 0 in (
             while if !Gready then false else limit > !counter
                do counter := !counter + 1 done;
             let size = 3 in
             let i = ref 0 in
             while size > !i do
                if cas(!Gtable @ !i, (!i % 4) + 1, 0)
                then i := !i + 1
                else if limit > !counter then error(wrong value in table)
                     else i := !i + 1
            done)";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
               (("ready", Boolean false)::(array_store 8)), false);

        (* Test 17 *)
        (* Producer/Consumer *)
        ([| "let size = 8 in
             let i = ref 0 in
             let limit = 2 in
             let ctr = ref 0 in
             let hash =
                fn x => ((((12773*x)%179)*7)%100)+1 in
             while limit > !ctr do
                if !(!Gtable @ !i) = 0
                then if cas(!Gtable @ (!i+1), 0, hash @ (!i+1))
                     then if cas(!Gtable @ !i, 0, 1)
                        then ctr := !ctr + 1; i := (!i + 2) % size
                        else error(marker should still be zero)
                     else error(supposed to be empty slot)
                else ctr := !ctr + 1
             done";

            "let size = 8 in
             let i = ref 0 in
             let limit = 2 in
             let ctr = ref 0 in
             while limit > !ctr do
                if !(!Gtable @ !i) = 1
                then let n = !(!Gtable @ (!i+1)) in
                   if n = 0 then error(supposed to be full slot)
                   else if cas(!Gtable @ (!i+1), n, 0)
                        then if cas(!Gtable @ !i, 1, 0)
                             then ctr := !ctr + 1; i := (!i + 2) % size
                             else error(marker should still be one)
                        else error(value changed since read)
                else ctr := !ctr + 1
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
               (array_store 8), false);

        (* Test 18 *)
        (* Set array values, and check they aren't set to values they aren't set to *)
       ([| "let table = !Gtable in
            let size = !Gsize in
            let mod = !Gmod in
            let tid = 1 in
            let i = ref 0 in
            while size > !i do
                if cas(table @ !i, 0, tid)
                then i := !i + 1
                else if !(table @ !i) % mod = 0
                     then error(should never be zero)
                     else i := !i + 1
            done";

           "let table = !Gtable in
            let size = !Gsize in
            let mod = !Gmod in
            let tid = 2 in
            let i = ref 0 in
            while size > !i do
                if cas(table @ !i, 0, tid)
                then i := !i + 1
                else if !(table @ !i) % mod = 0
                     then error(should never be zero)
                     else i := !i + 1
            done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
             ("size", Integer 4)::("mod", Integer 4)::(array_store 8), false);

        (* Beer in fridge example from Part IB Conc. Systems *)
        ([| "if !Gbeer then skip
             else if cas(Gbeer, false, true) then skip
                  else error(someone put beer in the fridge before me)";
            "if !Gbeer then skip
             else if cas(Gbeer, false, true) then skip
                  else error(someone put beer in the fridge before me)";
        |], [("beer", Boolean false)], true);

        (* Test 20 *)
        (* Solution to beer problem *)
        ([| "if !Gbeer then skip else
             ((if cas(Gnote, false, true)
             then if cas(Gbeer, false, true)
                  then skip
                  else error(someone bought beer before I did)
             else skip); cas(Gt1finished, false, true))";

            "if !Gbeer then skip else
             ((if cas(Gnote, false, true)
             then if cas(Gbeer, false, true)
                  then skip
                  else error(someone bought beer before I did)
             else skip); cas(Gt2finished, false, true))";

            "if !Gt1finished then if !Gt2finished
                then if !Gbeer then skip
                               else error(no beer at end)
                else skip else skip";

        |], [("beer", Boolean false); ("note", Boolean false);
             ("t1finished", Boolean false); ("t2finished", Boolean false)],
                false);

        (* Test 21 *)
        (* Another example showing off DPOR *)
        ([| "let size = !Gsize in
             let tid = 0 in
             let pi = ref 0 in
             while size > !pi do
                if cas(!Gtable @ !pi, 0, 2)
                then pi := !pi + 1
                else if !(!Gtable @ !pi) = tid
                     then error(set by another thread)
                     else pi := !pi + 1
            done";

            "let size = !Gsize in
             let tid = 1 in
             let pi = ref 0 in
             while size > !pi do
                if cas(!Gtable @ !pi, 0, 2)
                then pi := !pi + 1
                else if !(!Gtable @ !pi) = tid
                     then error(set by another thread)
                     else pi := !pi + 1
            done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
             ("size", Integer 3)::(array_store 100), false);

        (* Test 22 *)
        ([| "let size = !Gsize in
             let tid = 1 in
             let i = ref 0 in
             while size > !i do
                if cas(!Gtable @ !i, 0, 1)
                then if cas(!Gtable @ (!i + 1), 0, tid)
                     then if cas(!Gtable @ !i, 1, 2)
                          then i := !i + 2
                          else error(flag should be set to 1)
                     else error(value should be unset)
                else if !(!Gtable @ !i) = 2
                  then
                     let read = !(!Gtable @ (!i + 1)) in
                     if read = 0
                     then error(should be set by some thread)
                     else if read = tid
                     then error(should be set by another thread)
                     else i := !i + 2
                  else i := !i + 2
             done";

            "let size = !Gsize in
             let tid = 2 in
             let i = ref 0 in
             while size > !i do
                if cas(!Gtable @ !i, 0, 1)
                then if cas(!Gtable @ (!i + 1), 0, tid)
                     then if cas(!Gtable @ !i, 1, 2)
                          then i := !i + 2
                          else error(flag should be set to 1)
                     else error(value should be unset)
                else if !(!Gtable @ !i) = 2
                  then
                     let read = !(!Gtable @ (!i + 1)) in
                     if read = 0
                     then error(should be set by some thread)
                     else if read = tid
                     then error(should be set by another thread)
                     else i := !i + 2
                  else i := !i + 2
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
             ("size", Integer 2)::(array_store 8), false);

        (* Test 23 *)
        ([| "let size = !Gsize in
             let table = !Gtable in
             let i = ref 1 in
             while size > !i do
                if cas(table @ !i, 0, 1) then
                    i := !i + !Gincrement
                else error(should not be set)
            done";

            "let size = !Gsize in
             let table = !Gtable in
             let i = ref 2 in
             while size > !i do
                if cas(table @ !i, 0, 1) then
                    i := !i + !Gincrement
                else error(should not be set)
             done";

            "let size = !Gsize in
             let table = !Gtable in
             let i = ref 3 in
             while size > !i do
                if cas(table @ !i, 0, 1) then
                    i := !i + !Gincrement
                else error(should not be set)
             done";

         |], ("table", Pl_parser.expr_of_string int_to_loc_expr_str)::
             ("increment", Integer 3)::("size", Integer 4)::
             (array_store 100), false);
    ]

    let run_test (es, g, err_poss) =
        C.max_depth := 0; C.calls := 0;
        let convert e = (Pl_parser.expr_of_string e, C.ProgImp.ThrImp.StoreImp.empty) in
        let tds = Array.map convert es in
        let init_time = Sys.time() in
        if C.error_free (tds, g) = err_poss then (* Failure *)
             (print_string "Below initial state expected ";
              (if err_poss then print_string "to have error but none found\n"
                else print_string "not to have error but error found\n");
              print_endline (C.ProgImp.string_of_program (tds, g));
              print_newline ();
              false)
        else (print_endline ("Calls: " ^ (string_of_int !C.calls) ^ "\n"
                            ^" Time: " ^ (string_of_float
                                (Sys.time() -. init_time)) ^ "\n"
                            ^"Depth: " ^ (string_of_int !C.max_depth) ^ "\n");
              true) (* Success *)

    let all_tests_passed () =
        let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
            conj (List.mapi (fun index ->
                print_endline ("Index: " ^ (string_of_int index)); run_test)
                    test_cases)

    let print_all_tests_passed () = print_endline ("Checker Correctness Tests All Passed: "
                            ^ (string_of_bool (all_tests_passed ())) ^ "\n\n")
end

module SPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SimplePLChecker)

module DPORPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.DPORPLChecker)

let () = let n = if Array.length Sys.argv > 1
                 then int_of_string (Sys.argv.(1))
                 else 1 in
         if n = 0 then SPLCheckerCorrectnessTest.print_all_tests_passed()
    else if n = 1 then DPORPLCheckerCorrectnessTest.print_all_tests_passed()
    else print_endline "Error: pass 0 and 1 for simple or DPOR check tests"

