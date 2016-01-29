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
        let int_to_loc_str n =
            let rec itls_aux m = if m > n then "error(index out of bounds)"
                    else "if x=" ^ (string_of_int m) ^ " then G" ^
                            (string_of_int m) ^ " else " ^ (itls_aux (m+1))
            in "fn x => " ^ (itls_aux 0) in

    [
        (* Test 0 *)
        (* Initial expression strings, global store, error possible *)
        ([| "cas(Gx, 2, 0)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], false);

        (* Test 1 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], true);

        (* Test 2 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 2)";
         |], [("x", Integer 2)], false);

        (* Test 3 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 1, 1)";
         |], [("x", Integer 2)], false);

        (* Test 4 *)
        ([| "if cas(Gx, 1, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], true);

        (* Test 5 *)
        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Integer 5)], false);

        (* Test 6 *)
        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Skip)], true);

        (* Test 7 *)
        ([| "if cas(Gx, !Gx, 0) then skip else error(not cas)";
            "if cas(Gx, !Gx, 1) then skip else error(not cas)";
         |], [("x", Integer 2)], true);

        (* Test 8 *)
        ([| "if cas(Gx, !Gx, 2) then skip else error(not cas)";
            "if cas(Gx, !Gx, 2) then skip else error(not cas)";
         |], [("x", Integer 2)], false);

        (* Test 9 *)
        ([| "cas(Gx, !Gx, !Gx + 1)";
            "cas(Gx, !Gx, !Gx + 1)";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], true);

        (* Test 10 *)
        (* Exclude one thread from entering *)
        ([| "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], false);

        (* Test 11 *)
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

        (* Test 13 *)
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

        |], ("table", Pl_parser.expr_of_string (int_to_loc_str 128))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 128))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 128))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 8))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 8))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 8))::
             ("size", Integer 4)::("mod", Integer 4)::(array_store 8), false);

        (* Test 19 *)
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 100))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 8))::
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

         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 100))::
             ("increment", Integer 3)::("size", Integer 4)::
             (array_store 100), false);


        (* Test 24 *)
        (* Spinlocks *)
        ([| "lock SL0;
             let read = !Gx in
             if cas(Gx, read, read + 1)
                then unlock SL0
                else error(value unexpectedly changed)";

            "lock SL0;
             let read = !Gx in
             if cas(Gx, read, read + 1)
                then unlock SL0
                else error(value unexpectedly changed)";
        |], [("x", Integer 0)], false);

        (* Test 25 *)
        ([| "lock SL0;
             let read = !Gx in
             if cas(Gx, read, read + 1)
                then unlock SL0
                else error(value unexpectedly changed)";

            "let read = !Gx in
             if cas(Gx, read, read + 1)
                then skip
                else error(value unexpectedly changed)";
        |], [("x", Integer 0)], true);

        (* Test 26 *)
        ([| "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)";
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)";
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)";
        |], [("x", Boolean false)], false);

        (* Test 27 *)
        ([| "if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then skip
                  else error(value unexpectedly changed)
             else error(value should always be false)";
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)";
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)";
        |], [("x", Boolean false)], true);

        (* Test 28 *)
        ([| "let lf = fn i =>
                if i = 0 then SLl0 else
                if i = 1 then SLl1 else
                if i = 2 then SLl2 else error(out of bounds)
             in let ri = ref 0 in
             let rtot = ref 0 in
             while !ri < 3 do
                lock (lf @ !ri);
                rtot := !rtot + !(!Gtable @ !ri);
                ri := !ri + 1
             done;
             if ¬(!rtot % 3 = 0) then error(locking gone wrong)
             else while !ri > 0 do
                ri := !ri - 1;
                let location = (!Gtable @ !ri) in
                let vl = !location in
                if ¬cas(location, vl, vl) then error(unexpected change)
                else unlock (lf @ !ri)
            done";

            "let lf = fn i =>
                if i = 0 then SLl0 else
                if i = 1 then SLl1 else
                if i = 2 then SLl2 else error(out of bounds)
             in let ri = ref 0 in
             let rtot = ref 0 in
             while !ri < 3 do
                lock (lf @ !ri);
                rtot := !rtot + !(!Gtable @ !ri);
                ri := !ri + 1
             done;
             if ¬(!rtot % 3 = 0) then error(locking gone wrong)
             else while !ri > 0 do
                ri := !ri - 1;
                let location = (!Gtable @ !ri) in
                let vl = !location in
                if ¬cas(location, vl, vl) then error(unexpected change)
                else unlock (lf @ !ri)
            done";
         |], ("table", Pl_parser.expr_of_string (int_to_loc_str 4))::
             (array_store 4), false);

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

