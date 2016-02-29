module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let max_depth = ref 0
    let calls = ref 0
    (* See p.73 of Godefroid 1995 thesis *)
    let hsleep = Hashtbl.create 5000

    (* True iff error-free *)
    let rec check init_prog t_seq sleep_set =
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;

        let rec intersect l m  = match l with
            [] -> m
          | x::xs -> (match m with [] -> l
              | y::ys -> if x = y then x::(intersect xs ys)
                    else if x < y then intersect xs m
                    else intersect l ys) in
        let rec union l m = match l with
            [] -> m
          | x::xs -> (match m with 
                [] -> l
              | y::ys -> if x = y then x::(union xs ys)
                    else if x < y then x::(union xs m)
                    else y::(union l ys)) in
 
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in
        let curr_sleep = ref sleep_set in

        let to_explore = ref [] in
        (* (G95) if s is NOT already in H then *)
        if not (Hashtbl.mem hsleep (tds, g)) then (
            (* (G95) enter s (with sleep set) into H *)
            Hashtbl.add hsleep (tds, g) !curr_sleep;
            (* (G95) to_explore = persistent_set(s) \ s.Sleep *)
            for i = 0 to Array.length tds - 1 do
                if not (List.mem i !curr_sleep)
                then to_explore := i::!to_explore
            done
        (* (G95) else *)
        ) else (
            (* (G95) T = [t|t in H(s).Sleep & t not in s.Sleep] *)
            for i = 0 to Array.length tds - 1 do
                if List.mem i (Hashtbl.find hsleep (tds, g))
                    && not (List.mem i !curr_sleep)
                then to_explore := i::!to_explore
            done;
            (* (G95) s.Sleep = s.Sleep intersect H(s).Sleep *)
            curr_sleep := intersect !curr_sleep (Hashtbl.find hsleep (tds, g));
            (* (G95) H(s).Sleep = s.Sleep *)
            Hashtbl.replace hsleep (tds, g) !curr_sleep);

        let error_free = ref true in
        let one_thread_can_advance = ref false in
        let all_stopped_threads_are_values = ref true in
        let recursive_calls_deadlock_free = ref true in

        (* (G95) for all t in T do *)
        for i = 0 to Array.length tds - 1 do
            if List.mem i !to_explore then (
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              (* No futher transitions: check if this thread reaches a local error *)
              None -> let (reaches_err, reaches_nonval) =
                        ProgImp.ThrImp.check_local (e, s, g) in
                      if reaches_err then error_free := false;
                      if reaches_nonval then
                        all_stopped_threads_are_values := false
              (* There is a transition: check if error; explore from the new state *)
            | Some (t_tran, enabled) -> (
                if not enabled then all_stopped_threads_are_values := false else (
                one_thread_can_advance := true;
                (* (G95) s' = succ(s) after t *)
                (* (G95) s'.Sleep = {t' in s.Sleep | (t, t') independent} *)
                let next_sleep = ref [] in
                for j = 0 to Array.length tds - 1 do
                    if List.mem j !curr_sleep then
                    let (e', s') = Array.get tds j in
                    match ProgImp.ThrImp.next_transition (e',s',g) with
                        None -> () | Some (next_t', enabled') ->
                        if enabled' &&
                            not (next_t'.ProgImp.ThrImp.g_loc =
                                t_tran.ProgImp.ThrImp.g_loc) then
                        next_sleep := j::!next_sleep 
                done;
                (* (G95) push (s') onto stack (with sleep set) *)
                let (ef, df) = check init_prog (t_seq @ [(i, t_tran)]) (List.rev !next_sleep) in
                (* (G95) s.Sleep = s.Sleep union t *)
                curr_sleep := union !curr_sleep [i];
                error_free := !error_free && ef;
                   recursive_calls_deadlock_free := !recursive_calls_deadlock_free && df))
        ) done;
        (!error_free, !recursive_calls_deadlock_free &&
                (!one_thread_can_advance || !all_stopped_threads_are_values))

    let error_and_deadlock_free init_prog =
        Hashtbl.reset hsleep;
        check init_prog [] []
  end

module DPORChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    module T = ProgImp.ThrImp
    let max_depth = ref 0
    let calls = ref 0

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

        (* for all processes p *)
        for p = 0 to Array.length tds - 1 do

            let (e, s) = Array.get tds p in

            (* Is there next transition? *)
            match ProgImp.ThrImp.next_transition (e, s, g) with

              (* No futher transitions: check if this thread reaches a local error *)
              None -> let (reaches_err, reaches_nonval) =
                        ProgImp.ThrImp.check_local (e, s, g) in
                      if reaches_err then error_free := false;
                      if reaches_nonval then
                        all_stopped_threads_are_values := false

              (* There is a transition *)
            | Some (next_t, enabled) ->
                if enabled then
                    (* There is at least one transition to explore: this one *)
                    (one_thread_can_advance := true;
                    transition_to_explore := Some p)
                else all_stopped_threads_are_values := false;

                (* let i = L(alpha(next(s,p))) *)
                let i = last_ti next_t.T.g_loc in

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
        let n = Array.length tds in
        check (tds, g) [] (fun _ -> Clockvector.fresh n) (fun _ -> Clockvector.fresh n) (fun _ -> ~-1)
  end

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleChecker (Program.PLProgram)

module DPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = DPORChecker (Program.PLProgram)
