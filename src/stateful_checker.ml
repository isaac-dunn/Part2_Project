module SimpleStatefulChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    module ProgHashtbl = Hashtbl.Make(Prog)
    let do_stateful = true
    let write_error_traces = false
    let max_depth = ref 0
    let calls = ref 0
    let ht = ProgHashtbl.create 10000

    (* True iff error-free *)
    let rec check init_prog t_seq =
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in
        let error_free = ref true in
        let one_thread_can_advance = ref false in
        let all_stopped_threads_are_values = ref true in
        let recursive_calls_deadlock_free = ref true in
        if do_stateful && ProgHashtbl.mem ht (tds, g) then () else (
        if do_stateful then ProgHashtbl.add ht (tds, g) true;
        for i = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              (* No futher transitions: check if this thread reaches a local error *)
              None -> let (reaches_err, reaches_nonval) =
                        ProgImp.ThrImp.check_local (e, s, g) in
                      if reaches_err then (
                        error_free := false;
                        if write_error_traces then ProgImp.output_hasse_image
                            ("errortraces/errortrace" ^ (string_of_int !calls) ^ ".gv") t_seq
                      );
                      if reaches_nonval then
                         all_stopped_threads_are_values := false
              (* There is a transition: explore it if enabled *)
            | Some (t_tran, enabled) -> (
                if not enabled then all_stopped_threads_are_values := false else (
                one_thread_can_advance := true;
                let (ef, df) = check init_prog (t_seq @ [(i, t_tran)])
                in error_free := !error_free && ef;
                   recursive_calls_deadlock_free := !recursive_calls_deadlock_free && df))
        done);
        (!error_free, !recursive_calls_deadlock_free &&
                (!one_thread_can_advance || !all_stopped_threads_are_values))

    let error_and_deadlock_free init_prog =
        ProgHashtbl.reset ht;
        check init_prog []
  end

module SDPORChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    module ProgHashtbl = Hashtbl.Make(Prog)
    module T = ProgImp.ThrImp
    let do_stateful = true
    let max_depth = ref 0
    let calls = ref 0
    let ht = ProgHashtbl.create 10000
    let vodg = Hashtbl.create ~random:true 1000
    let write_error_traces = true

    (* Array for backtracking sets *)
    let backtracks = Var_array.empty()

    (* True iff error-free *)
    let rec check init_prog t_seq proc_cvs obj_cvs last_ti =
        (* Track execution info *)
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;

        let error_free = ref true in
        let one_thread_can_advance = ref false in
        let all_stopped_threads_are_values = ref true in
        let recursive_calls_deadlock_free = ref true in

        (* Used to determine if there is some enabled p *)
        let transition_to_explore = ref None in

        let pre i =
            let rec pre_aux tran_list index state =
                if index = i then state else
                    (pre_aux (List.tl tran_list) (index+1)
                        (ProgImp.apply_transition state (List.hd tran_list)))
            in pre_aux t_seq 0 init_prog in

        (* let s = last(S) *)
        let (tds, g) = pre depth in

        let update_backtrack_sets (p, next_tran) =
                (* let i = L(alpha(next(s,p))) *)
                let i = last_ti next_tran.T.g_loc in
                (* if i =/= ~-1 and not i <= C(p)(proc(Si)) *)
                if i > ~-1 then if
                   i > Clockvector.get (proc_cvs p) (fst (List.nth t_seq i))
                then (
                    let prei = pre i in
                    let is_enabled p' (tds', g') =
                        let (e', s') = Array.get tds' p' in
                        match ProgImp.ThrImp.next_transition (e', s', g') with
                        Some (_, b) -> b
                      | None -> false
                    (* if p in enabled(pre(S, i)) *)
                    in if is_enabled p prei
                    (* then add p to backtrack(pre(S, i)) *)
                    then Var_array.set backtracks i (p::(Var_array.get backtracks i))
                    (* else add enabled(pre(S,i)) to backtrack(pre(S,i)) *)
                    else let rec ntoz n = if n = 0 then [0] else n::(ntoz (n-1))
                         in let en_in_prei q = is_enabled q prei
                         in Var_array.set backtracks i (List.filter en_in_prei
                                (ntoz (Array.length tds - 1)))
                    )
        in

        if do_stateful && ProgHashtbl.mem ht (tds, g) then (
        (* For each transition enabled from s *)
        for p = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds p in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              None -> ()
            | Some (next_t, enabled) -> if not enabled then () else
                (* Do depth first search of G starting at next_t *)
                let visited = Hashtbl.create 100 in
                let rec search_and_update t =
                    if Hashtbl.mem visited t then () else (
                    Hashtbl.add visited t true;
                    update_backtrack_sets t;
                    let next_ts = if Hashtbl.mem vodg t then Hashtbl.find vodg t
                                    else [] in
                    List.iter search_and_update next_ts)
                (* Update backtracking sets for each node reachable graph node *)
                in search_and_update (p, next_t)
        done);
        if do_stateful then ProgHashtbl.add ht (tds, g) true;

        (* for all processes p *)
        for p = 0 to Array.length tds - 1 do

            let (e, s) = Array.get tds p in

            (* Is there next transition? *)
            match ProgImp.ThrImp.next_transition (e, s, g) with

              (* No futher transitions: check if this thread reaches a local error *)
              None -> let (reaches_err, reaches_nonval) =
                        ProgImp.ThrImp.check_local (e, s, g) in
                      if reaches_err then (
                        error_free := false;
                        if write_error_traces then ProgImp.output_hasse_image
                            ("errortraces/errortrace" ^ (string_of_int !calls) ^ ".gv") t_seq
                      );
                      if reaches_nonval then
                        all_stopped_threads_are_values := false

              (* There is a transition *)
            | Some (next_t, enabled) ->
                if enabled then
                    (* There is at least one transition to explore: this one *)
                    (one_thread_can_advance := true;
                    transition_to_explore := Some p)
                else all_stopped_threads_are_values := false;
                update_backtrack_sets (p, next_t)
        done;

        (* if there is some p in enabled(s) *)
        (* As we have no locks, all available transitions are enabled *)
        (match !transition_to_explore with None -> () | Some pi -> (

        (* backtrack(s) := {p} *)
        Var_array.append backtracks [pi];

        let rec find_p bs ds = match bs with
            [] -> None
          | x::xs -> if List.mem x ds then find_p xs ds else Some x in

        (* let done = emptyset *)
        (* while there is some p in backtrack(s) but not done *)
        let rec whileloop po dones = match po with None -> () | Some p -> ((

        let (e, s) = Array.get tds p in
        match T.next_transition (e, s, g) with
            None -> raise (Failure "Should NEVER happen; there is a transition")
          | Some (next_t, enabled) -> (

        if not enabled then raise (Failure "Only enabled transitions should be explored");

        (* let S' = S.next(s,p) *)
        let new_t_seq = t_seq @ [(p, next_t)] in

        (* let o = alpha(next(s,p)) *) 
        let o = next_t.T.g_loc in

        (* let cv = max(C(p),C(o))[p:=|S'|] *)
        let cv = Clockvector.max (proc_cvs p) (obj_cvs o) in
        Clockvector.set cv p (List.length new_t_seq);

        (* let C' = C[p:=cv, o:=cv] *)
        let new_proc_cvs pi = if pi = p then cv else proc_cvs pi in
        let new_obj_cvs oi = if oi = o then cv else obj_cvs oi in

        (* let L' = L[o:=|S'|] *)
        let new_last_ti oi = if oi = o then List.length new_t_seq - 1 (* index *)
                                       else last_ti oi in

        (* Add appropriate edges to G *)
        (* If t_seq isn't empty then add edge in G from
           last transition in t_seq to the new transition*)
        if t_seq <> [] then (
            let last = List.hd (List.rev t_seq) in
            let new_range = if Hashtbl.mem vodg last
                    then let old_r = Hashtbl.find vodg last
                        in if List.mem (p, next_t) old_r then old_r
                                else (p, next_t)::old_r
                    else [(p, next_t)] in
                 Hashtbl.replace vodg last new_range);

        (* Explore(S', C', L') *)
        let (ef, df) = check init_prog new_t_seq new_proc_cvs new_obj_cvs new_last_ti in
        error_free := !error_free && ef;
        recursive_calls_deadlock_free := !recursive_calls_deadlock_free && df;

        (* go back to top of while loop if necessary *)
        (* also contains add p to done *)
        )); whileloop (find_p (Var_array.get backtracks (Var_array.length backtracks - 1)) (p::dones)) (p::dones))
        (* also contains let done = emptyset *)
        in whileloop (find_p (Var_array.get backtracks (Var_array.length backtracks - 1)) []) [];
            Var_array.remove_last backtracks (* Decrement size of array to counter earlier increment *)));
 
        (!error_free, !recursive_calls_deadlock_free &&
            (!one_thread_can_advance || !all_stopped_threads_are_values))

    let error_and_deadlock_free (tds, g) =
        ProgHashtbl.reset ht;
        Hashtbl.reset vodg;
        let n = Array.length tds in
        check (tds, g) [] (fun _ -> Clockvector.fresh n) (fun _ -> Clockvector.fresh n) (fun _ -> ~-1)
  end

module SimpleStatefulPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleStatefulChecker (Program.PLProgram)

module SDPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SDPORChecker (Program.PLProgram)

