(* Unit tests for language for Part II Project *)
(* Isaac Dunn 15/11/2015 *)

open Pl_expression

module PLCorrectnessTest (Chk :
    Interfaces.Checker with module ProgImp = Program.PLProgram) = struct

    module C = Chk

    type case = (string array * C.ProgImp.ThrImp.StoreImp.store * (bool * bool))

    let test_cases =
        let rec array_store n loc_fun init_val_fun = if n < 0 then []
            else  (loc_fun n, init_val_fun n)::
                    (array_store (n-1) loc_fun init_val_fun) in
        let intloc_array_zero_store n =
            array_store n string_of_int (fun _ -> Integer 0) in
        let array_pl_fun n loc_fun =
            let rec itls_aux m = if m > n then "error(index out of bounds)"
                    else "if x=" ^ (string_of_int m) ^ " then " ^ (loc_fun m)
                              ^ " else " ^ (itls_aux (m+1))
            in "fn x => " ^ (itls_aux 0) in
        let int_to_loc_str n = array_pl_fun n (fun m -> "G" ^ (string_of_int m)) in

    [
        (* Initial expression strings, global store, (error free, deadlock free) *)

        (* Test 0 *)
        ([| "cas(Gx, 2, 0)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], (true, true));

        (* Test 1 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], (false, true));

        (* Test 2 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 2, 2)";
         |], [("x", Integer 2)], (true, true));

        (* Test 3 *)
        ([| "if cas(Gx, 2, 0) then skip else error(not cas)";
            "cas(Gx, 1, 1)";
         |], [("x", Integer 2)], (true, true));

        (* Test 4 *)
        ([| "if cas(Gx, 1, 0) then skip else error(not cas)";
            "cas(Gx, 2, 1)";
         |], [("x", Integer 2)], (false, true));

        (* Test 5 *)
        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Integer 5)], (true, true));

        (* Test 6 *)
        ([| "while cas (Gx, false, skip) do error(unreachable) done";
            "while cas (Gx, skip, true) do error(unreachable) done";
         |], [("x", Skip)], (false, true));

        (* Test 7 *)
        ([| "if cas(Gx, !Gx, 0) then skip else error(not cas)";
            "if cas(Gx, !Gx, 1) then skip else error(not cas)";
         |], [("x", Integer 2)], (false, true));

        (* Test 8 *)
        ([| "if cas(Gx, !Gx, 2) then skip else error(not cas)";
            "if cas(Gx, !Gx, 2) then skip else error(not cas)";
         |], [("x", Integer 2)], (true, true));

        (* Test 9 *)
        ([| "cas(Gx, !Gx, !Gx + 1)";
            "cas(Gx, !Gx, !Gx + 1)";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], (false, true));

        (* Test 10 *)
        (* Exclude one thread from entering *)
        ([| "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if cas(Gl, 0, 1) then cas(Gx, !Gx, !Gx + 1) else skip";
            "if !Gx > 1 then error(both threads accessed x) else skip";
         |], [("l", Integer 0); ("x", Integer 0)], (true, true));

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
            (true, true));

        (* Test 12 *)
        (* Large state space *)
        ([| "let r = ref 0 in
                while 2 > !r do if cas(Gx, !r, !r+1) then r := !Gx else r := !Gx done"; 
            "let r= ref 0 in
                while 2 > !r do if cas(Gx, !r, !r+1) then r := !Gx else r := !Gx done"; 
        |], [("x", Integer 0)], (true, true));

        (* Test 13 *)
        (* Indexer example *)
        (let test13_thread n =
            "let size= 128 in
             let max = 4 in
             let tid = " ^ (string_of_int n) ^ " in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let m = ref 0 in
             let w = ref 0 in
             let h = ref 0 in
             let hash = fn x => (x * 7) % size in
             let getmsg = fn u => !m * 11 + tid in
             while max > !m do
                m := !m + 1;
                w := (getmsg @ skip);
                h := (hash @ !w);
                while (cas(table @ !h, 0, !w) = false) do
                    h := (!h + 1) % size
                done
            done" in

        (Array.init 2 test13_thread, (intloc_array_zero_store 128),
        (true, true)));

        (* Test 14 *)
        (* Thread 0 accesses even array indices; thread 1 accesses odd *)
        (let test14_thread n =
            "let size = 128 in
             let max = 4 in
             let tid = " ^ (string_of_int n) ^ " in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let i = ref 0 in
             while max > !i do
                if cas(table @ (tid + 2 * !i), 0, 1)
                then i := !i + 1
                else error(array element already reached)
             done" in

        (* N.B. Possible error if more than 2 threads *)
        (Array.init 2 test14_thread, (intloc_array_zero_store 128), (true, true)));

        (* Test 15 *)
        (* Thread 0 accesses 0,3,6,9; thread 1 accesses 1,5,9; error at 9 *)
        ([| "let size = 128 in
             let max = 4 in
             let tid = 0 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let i = ref 0 in
             while max > !i do
                if cas(table @ (tid + 3 * !i), 0, 1)
                then i := !i + 1
                else error(array element already reached)
             done";

            "let size = 128 in
             let max = 4 in
             let tid = 1 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let i = ref 0 in
             while max > !i do
                if cas(table @ (tid + 4 * !i), 0, 1)
                then i := !i + 1
                else i := !i + 1
             done";

         |], (intloc_array_zero_store 128), (false, true));

        (* Test 16 *)
        (* first set array values in thread 0, then check them in thread 1 *)
        ([| "let size = 3 in
             let i = ref 0 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             while size > !i do
                if cas(table @ !i, 0, (!i % 4) + 1)
                then i := !i + 1
                else error(array should be initially zero)
             done; if cas(Gready, false, true) then skip
                    else error(should only be ready now not before)";

            "let limit = 8 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let counter = ref 0 in (
             while if !Gready then false else limit > !counter
                do counter := !counter + 1 done;
             let size = 3 in
             let i = ref 0 in
             while size > !i do
                if cas(table @ !i, (!i % 4) + 1, 0)
                then i := !i + 1
                else if limit > !counter then error(wrong value in table)
                     else i := !i + 1
            done)";

         |], (("ready", Boolean false)::(intloc_array_zero_store 128)), (true, true));

        (* Test 17 *)
        (* Producer/Consumer *)
        ([| "let size = 8 in
             let i = ref 0 in
             let limit = 2 in
             let ctr = ref 0 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let hash =
                fn x => ((((12773*x)%179)*7)%100)+1 in
             while limit > !ctr do
                if !(table @ !i) = 0
                then if cas(table @ (!i+1), 0, hash @ (!i+1))
                     then if cas(table @ !i, 0, 1)
                        then ctr := !ctr + 1; i := (!i + 2) % size
                        else error(marker should still be zero)
                     else error(supposed to be empty slot)
                else ctr := !ctr + 1
             done";

            "let size = 8 in
             let i = ref 0 in
             let limit = 2 in
             let ctr = ref 0 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             while limit > !ctr do
                if !(table @ !i) = 1
                then let n = !(table @ (!i+1)) in
                   if n = 0 then error(supposed to be full slot)
                   else if cas(table @ (!i+1), n, 0)
                        then if cas(table @ !i, 1, 0)
                             then ctr := !ctr + 1; i := (!i + 2) % size
                             else error(marker should still be one)
                        else error(value changed since read)
                else ctr := !ctr + 1
             done";

         |], (intloc_array_zero_store 128), (true, true));

        (* Test 18 *)
        (* Set array values, and check they aren't set to values they aren't set to *)
        (let test18_thread n =
           "let table = " ^ (int_to_loc_str 128) ^ " in
            let size = 6 in
            let tid = " ^ (string_of_int (n+1)) ^ " in
            let i = ref 0 in
            while size > !i do
                if cas(table @ !i, 0, tid)
                then i := !i + 1
                else let read = !(table @ !i) in
                    if ((read = 0) | (read = tid))
                    then error(should be the tid of another thread)
                    else i := !i + 1
            done" in

        (Array.init 2 test18_thread, (intloc_array_zero_store 8), (true, true)));

        (* Test 19 *)
        (* Beer in fridge example from Part IB Conc. Systems *)
        ([| "if !Gbeer then skip
             else if cas(Gbeer, false, true) then skip
                  else error(someone put beer in the fridge before me)";
            "if !Gbeer then skip
             else if cas(Gbeer, false, true) then skip
                  else error(someone put beer in the fridge before me)";
        |], [("beer", Boolean false)], (false, true));

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
                (true, true));

        (* Test 21 *)
        (* Another example showing off DPOR *)
        (* On reflection very similar to test 18 *)
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
             ("size", Integer 3)::(intloc_array_zero_store 100), (true, true));

        (* Test 22 *)
        (* Use index 2k as mutex flag for index 2k+1 *)
        (let test22_thread n =
            "let size = 3 in
             let tid = " ^ (string_of_int (n+1)) ^ " in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let i = ref 0 in
             while size > !i do
                if cas(table @ !i, 0, 1)
                then if cas(table @ (!i + 1), 0, tid)
                     then if cas(table @ !i, 1, 2)
                          then i := !i + 2
                          else error(flag should be set to 1)
                     else error(value should be unset)
                else if !(table @ !i) = 2
                  then
                     let read = !(table @ (!i + 1)) in
                     if read = 0
                     then error(should be set by some thread)
                     else if read = tid
                     then error(should be set by another thread)
                     else i := !i + 2
                  else i := !i + 2
             done" in

        (Array.init 2 test22_thread, (intloc_array_zero_store 8), (true, true)));

        (* Test 23 *)
        (* Thread i sets indices j such that j === i (mod num_threads) *)
        (let num_threads = 2 in
         let test23_thread n =
            "let size = 10 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let i = ref " ^ (string_of_int n) ^ " in
             while size > !i do
                if cas(table @ !i, 0, 1) then
                    i := !i + " ^ (string_of_int num_threads) ^ "
                else error(should not be set)
            done" in

        (Array.init num_threads test23_thread,
             (intloc_array_zero_store 128), (true, true)));


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
        |], [("x", Integer 0); ("0", Boolean false); ("1", Boolean false);],
            (true, true));

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
        |], [("x", Integer 0); ("0", Boolean false);], (false, true));

        (* Test 26 *)
        (Array.make 7
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)"
        ,[("x", Boolean false); ("0", Boolean false);], (true, true));

        (* Test 27 *)
        (* One thread doesn't bother locking *)
        (Array.append
         [| "if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then skip
                  else error(value unexpectedly changed)
             else error(value should always be false)";|]
         (Array.make 3
            "lock SL0;
             if cas(Gx, false, true)
             then if cas(Gx, true, false)
                  then unlock SL0
                  else error(value unexpectedly changed)
             else error(value should always be false)")
        , [("x", Boolean false); ("0", Boolean false);], (false, false));

        (* Test 28 *)
        (Array.make 2 (
            "let lf = fn i =>
                if i = 0 then SLl0 else
                if i = 1 then SLl1 else
                if i = 2 then SLl2 else error(out of bounds)
             in let ri = ref 0 in
             let table = " ^ (int_to_loc_str 128) ^ " in
             let rtot = ref 0 in
             while !ri < 3 do
                lock (lf @ !ri);
                rtot := !rtot + !(table @ !ri);
                ri := !ri + 1
             done;
             if ¬(!rtot % 3 = 0) then error(locking gone wrong)
             else while !ri > 0 do
                ri := !ri - 1;
                let location = (table @ !ri) in
                let vl = !location in
                if ¬cas(location, vl, vl) then error(unexpected change)
                else unlock (lf @ !ri)
            done")
         , [("l0", Boolean false); ("l1", Boolean false); ("l2", Boolean false);]
            @ intloc_array_zero_store 128, (true, true));

        (* Test 29 *)
        (* Deadlocks can happen *)
        ([| "lock SL0; lock SL1; unlock SL1; unlock SL0";
            "lock SL1; lock SL0; unlock SL0; unlock SL1";
        |], [("0", Boolean false);("1", Boolean false);], (true, false));

        (* Test 30 *)
        (* But an ordering on the locks can avoid it *)
        ([| "lock SL0; lock SL1; unlock SL1; unlock SL0";
            "lock SL0; lock SL1; unlock SL1; unlock SL0";
        |], [("0", Boolean false);("1", Boolean false);], (true, true));

        (* Test 31 *)
        (* Forgot to unlock, oops *)
        ([| "lock SL0; lock SL1; unlock SL1; unlock SL0";
            "lock SL0; lock SL1; unlock SL1";
        |], [("0", Boolean false);("1", Boolean false);], (true, false));

        (* Test 32 *)
        ([| "lock SL1; lock SL0; unlock SL0; unlock SL1";
            "lock SL1; lock SL0; unlock SL1";
        |], [("0", Boolean false);("1", Boolean false);], (true, false));

        (* Test 33 *)
        ([| "lock SL1; lock SL0; unlock SL1";
            "lock SL1; lock SL0; unlock SL0; unlock SL1";
        |], [("0", Boolean false);("1", Boolean false);], (true, false));

        (* Test 34 *)
        (* File System Example from POPL'05 *)
        (let test34_thread n =
           "let i = " ^ (string_of_int n) ^ " % 32 in
            let numblocks = 26 in
            let locki = " ^ (array_pl_fun 32 (fun i -> "SLi"^(string_of_int i)))
            ^ " in let inode = " ^ (array_pl_fun 32 (fun i -> "Ginode_"^(string_of_int i)))
            ^ " in let lockb = " ^ (array_pl_fun 26 (fun i -> "SLb"^(string_of_int i)))
            ^ " in let busy = " ^ (array_pl_fun 26 (fun i -> "Gbusy_"^(string_of_int i)))
            ^ " in
            lock (locki @ i);
            (if !(inode @ i) = 0
            then let b = ref ((i*2) % numblocks) in
                 let loop = ref true in
                 while !loop do
                    lock (lockb @ !b);
                    if ¬(!(busy @ !b))
                    then if cas(busy @ !b, false, true)
                         then if cas(inode @ i, 0,!b+1)
                              then unlock (lockb @ !b);
                                   loop := false
                              else error(we have inode lock)
                         else error(we have busy lock)
                    else unlock (lockb @ !b);
                         b := (!b + 1) % numblocks
                done
            else skip);
            unlock (locki @ i)" in

        (Array.init 2 test34_thread,
              (array_store 32 (fun i -> "i"^(string_of_int i))
                        (fun _ -> Boolean false))
            @ (array_store 26 (fun i -> "b"^(string_of_int i))
                        (fun _ -> Boolean false))
            @ (array_store 32 (fun i -> "inode_"^(string_of_int i))
                        (fun _ -> Integer 0))
            @ (array_store 26 (fun i -> "busy_"^(string_of_int i))
                        (fun _ -> Boolean false)),
        (true, true)));

        (* Test 35 *)
        (* Reads only *)
        ([| "if !Ga + !Gb + !Gc + !Gd = 33
             then skip else error(should be 33)";

            "if !Ga + !Gb + !Gc + !Gd = 33
             then skip else error(should be 33)";

            "if !Ga + !Gb + !Gc + !Gd = 33
             then skip else error(should be 33)";

        |], [("a", Integer 10); ("b", Integer 11);
             ("c", Integer 12); ("d", Integer 0);], (true, true));

        (* Test 36 *)
        (* Repetitive reading *)
        ([| "if !Ga + !Ga + !Ga + !Ga = 44 then skip else error(should be 33)";
            "if !Ga + !Ga + !Ga + !Ga = 44 then skip else error(should be 33)";
            "if !Ga + !Ga + !Ga + !Ga = 44 then skip else error(should be 33)";
        |], [("a", Integer 11)], (true, true));

        (* Test 37 *)
        (* Multiple producer & consumers, using locks *)
        (let buf_size = 8 in
         let limit = 3 in
         let num_each = 1 in
         let common_prefix = (
           "let bufsize = " ^ (string_of_int buf_size) ^ " in
            let i = ref 0 in
            let limit = " ^ (string_of_int limit) ^ " in
            let ctr = ref 0 in
            let hash = fn x => (((((x*x*292)/37)*(1+(x%3)))*x)%777) + 1 in
            let items = " ^
                (array_pl_fun buf_size (fun i -> "Gitem"^(string_of_int i))) ^
           " in let locks = " ^
                (array_pl_fun buf_size (fun i -> "SLitemlock"^(string_of_int i))) ^
           " in while !ctr < limit do
                lock (locks @ !i);
           ") in
         let prod_thread = common_prefix ^ (
              "(if !(items @ !i) = 0 then
                    if cas(items @ !i, 0, hash @ !i)
                    then skip else error(unexpectedly written to)
                else skip);
                unlock (locks @ !i);
                i := (!i + 1) % bufsize;
                ctr := !ctr + 1
            done") in
         let cons_thread = common_prefix ^ (
               "(let read = !(items @ !i) in
                 if read > 0 then
                    if cas(items @ !i, read, 0)
                    then skip else error(unexpectedly written to)
                 else skip);
                unlock (locks @ !i);
                i := (!i + 1) % bufsize;
                ctr := !ctr + 1
            done") in
        (Array.append
            (Array.make num_each prod_thread) (Array.make num_each cons_thread)
        , array_store buf_size (fun i -> "item"^(string_of_int i))
                        (fun _ -> Integer 0)
        @ array_store buf_size (fun i -> "itemlock"^(string_of_int i))
                        (fun _ -> Boolean false)
        , (true, true)));

        (* Test 38 *)
        (* Shows that naive stateful DPOR is not sound *)
        (* Whether Ga is set or not, the same state should be reached, but a = 2
           needs to insert a backtracking point, while a = 1 does not *)
        ([| "if ¬(!Gready1) then skip else
             if !Ga = 1 then if cas(Ggotoerr, true, false) then cas(Gready2, false, true) else cas(Gready2, false, true)
                        else skip";
            "if ¬(!Gready1) then skip else
             if !Ga = 1 & ¬(!Gready2) then skip else if !Ggotoerr then error(gte) else skip";
            "cas(Ggotoerr, true, false)";
            "if cas(Ga, 0, 1) then skip else
                if cas(Gready1, false, true) then skip
                else error(this should never happen)";
            "if cas(Ga, 0, 2) then skip else
                if cas(Gready1, false, true) then skip
                else error(this should never happen)";
        |], [("gotoerr", Boolean true); ("ready1", Boolean false); ("ready2", Boolean true);
             ("a", Integer 0);], (false, true));

        (* Test 39 *)
        (* Fig. 1 from SPIN '08 stateful DPOR paper *)
        ([| "let i = ref 0 in
             let n = !Gn in
             while !i < n do
                let rd = !Gd in
                if cas(Gd, rd, rd + !i)
                then if (rd + !i) % 5 = 4 then
                    error(assert this never true)
                    else i := !i + 5
                else skip
             done";
            "let j = ref 0 in
             let n = !Gn in
             while !j < n do
                let rd = !Gd in
                if cas(Gd, rd, rd - !j)
                then if (rd - !j) % 5 = 4 then
                    error(assert this never true)
                    else j := !j + 2
                else skip
             done";
        |], [("d", Integer 0); ("n", Integer 9)], (true, true));

        (* Test 40 *)
        (* Fig. 10 from Cartesian POR by Gueta et al. *)
        (let n = 3 in
        ([| "let N = " ^ (string_of_int n) ^ " in
             let A = " ^ (array_pl_fun n
                (fun i -> "Garray"^(string_of_int i))) ^ " in
             while !Gidx0 < N do
                lock SLatom;
                let read = !Gidx0 in
                if cas(A @ read, !(A @ read), !Gcounter + read)
                then if cas(Gidx0, read, read + 2) then unlock SLatom
                else error(has lock so no interruptions expected)
                else error(has lock so no interruptions expected)
            done;
            lock SLatom;
            let read = !Gcounter in
            if cas(Gcounter, read, read + 1 + !Gidx1)
            then (if !Gcounter > 2*N + 4
            then error(assertion fail)
            else unlock SLatom)
            else error(unexpectedly changed from read value)";
            "let N = " ^ (string_of_int n) ^ " in
             let A = " ^ (array_pl_fun n
                (fun i -> "Garray"^(string_of_int i))) ^ " in
             while !Gidx1 < N do
                lock SLatom;
                let read = !Gidx1 in
                if cas(A @ read, !(A @ read), !Gcounter + read)
                then if cas(Gidx1, read, read + 2) then unlock SLatom
                else error(has lock so no interruptions expected)
                else error(has lock so no interruptions expected)
            done;
            lock SLatom;
            let read = !Gcounter in
            if cas(Gcounter, read, read + 1 + !Gidx0)
            then (if !Gcounter > 2*N + 4
            then error(assertion fail)
            else unlock SLatom)
            else error(unexpectedly changed from read value)";
        |], [("idx0", Integer 0); ("idx1", Integer 1);
             ("counter", Integer 1); ("atom", Boolean false);]
            @ (array_store n (fun i -> "array"^(string_of_int i)))
                    (fun _ -> Integer 0), (true, true)));

        (* Test 41 *)
        (* Dining Philosophers Unsolved *)
        (let n = 2 in
         let test41_thread i =
            "lock SL" ^ (string_of_int i) ^ ";
             lock SL" ^ (string_of_int 
                (if i = 0 then (n-1) else i-1)) ^ ";
             unlock SL" ^ (string_of_int
                (if i = 0 then (n-1) else i-1)) ^ ";
             unlock SL" ^ (string_of_int i) in
         ((Array.init n test41_thread),
            array_store n (fun i -> (string_of_int i))
                (fun _ -> Boolean false),
          (true, false)));

        (* Test 42 *)
        (* Dining Philosophers Solved *)
        (let n = 2 in
         let test42_thread i =
            "let i = " ^ (string_of_int i) ^ " in
             if i % 2 = 0 then
                lock SL" ^ (string_of_int i) ^ ";
                lock SL" ^ (string_of_int
                    (if i = 0 then (n-1) else i-1)) ^ ";
                unlock SL" ^ (string_of_int
                    (if i = 0 then (n-1) else i-1)) ^ ";
                unlock SL" ^ (string_of_int i) ^ "
             else
                lock SL" ^ (string_of_int
                    (if i = 0 then (n-1) else i-1)) ^ ";
                lock SL" ^ (string_of_int i) ^ ";
                unlock SL" ^ (string_of_int i) ^ ";
                unlock SL" ^ (string_of_int
                    (if i = 0 then (n-1) else i-1)) in
         ((Array.init n test42_thread),
            array_store n (fun i -> (string_of_int i))
                (fun _ -> Boolean false),
          (true, true)));

    ]

    let call_counter = ref 0

    let run_test (es, g, (eef, edf)) =
        C.max_depth := 0; C.calls := 0;
        let convert e = (Pl_parser.expr_of_string e, C.ProgImp.ThrImp.StoreImp.empty) in
        let tds = Array.map convert es in
        let init_time = Sys.time() in
        let (aef, adf) = C.error_and_deadlock_free (tds, g) in
        let success = aef = eef && adf = edf in
        if success then print_endline "Result: Success"
                   else (print_string "Result: Failure";
                    if aef <> eef then if eef
                        then print_string " (Unexpected error)"
                        else print_string " (Unexpectedly error-free)";
                    if adf <> edf then if edf
                        then print_string " (Unexpected deadlock)"
                        else print_string " (Unexpectedly deadlock-free)";
                    print_newline());
        call_counter := !call_counter + !C.calls;
        print_endline (" Calls: " ^ (string_of_int !C.calls) ^ "\n"
        ^"  Time: " ^ (string_of_float
            (Sys.time() -. init_time)) ^ "\n"
        ^" Depth: " ^ (string_of_int !C.max_depth) ^ "\n");
        success

    let run_nth_test_case n = let _ = run_test (List.nth test_cases n) in ()

    let all_tests_passed () =
        call_counter := 0;
        let rec conj l = match l with [] -> true | b::bs -> b && conj bs in
            conj (List.mapi (fun index ->
                print_endline (" Index: " ^ (string_of_int index)); run_test)
                    test_cases)

    let print_all_tests_passed () =
            print_endline ("Checker Correctness Tests All Passed: "
                            ^ (string_of_bool (all_tests_passed ())));
            print_endline ("Total calls: " ^ (string_of_int !call_counter))
end

module SPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SimplePLChecker)

module DPORPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.DPORPLChecker)

module SPORPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SPORPLChecker)

module SPORSleepPLCheckerCorrectnessTest = PLCorrectnessTest (Checker.SPORSleepPLChecker)

let () = let n = if Array.length Sys.argv > 1
                 then int_of_string (Sys.argv.(1))
                 else 1 in
    let start_time = Sys.time () in
    if Array.length Sys.argv = 3 then
         if n = 0 then SPLCheckerCorrectnessTest.run_nth_test_case
                                (int_of_string Sys.argv.(2))
         else if n = 1 then DPORPLCheckerCorrectnessTest.run_nth_test_case
                                (int_of_string Sys.argv.(2))
         else if n = 2 then SPORPLCheckerCorrectnessTest.run_nth_test_case
                                (int_of_string Sys.argv.(2))
         else if n = 3 then SPORSleepPLCheckerCorrectnessTest.run_nth_test_case
                                (int_of_string Sys.argv.(2))
         else if n = 9 then let tc = (int_of_string Sys.argv.(2)) in
                print_endline "\nSimple";
                SPLCheckerCorrectnessTest.run_nth_test_case tc;
                print_endline "\nDPOR";
                DPORPLCheckerCorrectnessTest.run_nth_test_case tc;
                print_endline "\nSPOR";
                SPORPLCheckerCorrectnessTest.run_nth_test_case tc;
                print_endline "\nSPOR with sleep sets";
                SPORSleepPLCheckerCorrectnessTest.run_nth_test_case tc
         else print_endline ("Error: pass 0 or 1 or 2 or 3 for " ^
            "simple or DPOR or SPOR or SPOR with sleep check tests, " ^
            " or 9 to run all checkers")
    else if n = 0 then SPLCheckerCorrectnessTest.print_all_tests_passed()
         else if n = 1 then DPORPLCheckerCorrectnessTest.print_all_tests_passed()
         else if n = 2 then SPORPLCheckerCorrectnessTest.print_all_tests_passed()
         else if n = 3 then SPORSleepPLCheckerCorrectnessTest.print_all_tests_passed()
         else print_endline ("Error: pass 0 or 1 or 2 or 3 for " ^
            "simple or DPOR or SPOR or SPOR with sleep check tests");
    print_endline ("Time taken: " ^ (string_of_float (Sys.time () -. start_time)))

