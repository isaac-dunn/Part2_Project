let rec intersect l m  = match l with
    [] -> []
  | x::xs -> (match m with [] -> []
      | y::ys -> if x = y then x::(intersect xs ys)
            else if x < y then intersect xs (y::ys)
            else intersect (x::xs) ys)

let rec union l m = match l with
    [] -> m
  | x::xs -> (match m with
        [] -> l
      | y::ys -> if x = y then x::(union xs ys)
            else if x < y then x::(union xs (y::ys))
            else y::(union (x::xs) ys))

module SPORChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let write_error_traces = false
    let max_depth = ref 0
    let calls = ref 0
    let ht = Hashtbl.create ~random:true 10000
    let locs_table = ref (Array.make 0 [])

    let persistent_set (tds, g) =
        let sbset xs ys = List.for_all (fun x -> List.mem x ys) xs in
        let eq xs ys = sbset xs ys && sbset ys xs in
        let locs_accsd n = Array.get !locs_table n in
        let is_enabled n =
            let (e, s) = Array.get tds n in
            match ProgImp.ThrImp.next_transition (e, s, g) with
            Some (_, b) -> b
          | None -> false in
        let enabled_index =
            let i = ref 0 in
            while not (is_enabled !i) && !i < Array.length tds -1
            do i := !i + 1 done;
            !i in
        let old_pers = ref [] in
        let pers = ref [enabled_index] in
        let locs = ref (locs_accsd 0) in
        while not (eq !pers !old_pers) do
            old_pers := !pers;
            for i = 0 to Array.length tds - 1 do
                if not (List.mem i !pers) then
                if List.exists (fun l -> List.mem l !locs) (locs_accsd i)
                then (locs := union (locs_accsd i) !locs; pers := i::!pers)
            done
        done;
        if List.for_all is_enabled !pers then !pers else
            let rec ntoz n = if n < 0 then [] else n::(ntoz (n-1)) in
            ntoz (Array.length tds - 1)

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
        if Hashtbl.mem ht (tds, g) then () else (
        Hashtbl.add ht (tds, g) true;
        let persists = persistent_set (tds, g) in
        for i = 0 to Array.length tds - 1 do
            if List.mem i persists then (
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
              (* There is a transition: check if error; explore from the new state *)
            | Some (t_tran, enabled) -> (
                if not enabled then all_stopped_threads_are_values := false else (
                one_thread_can_advance := true;
                let (ef, df) = check init_prog (t_seq @ [(i, t_tran)])
                in error_free := !error_free && ef;
                   recursive_calls_deadlock_free := !recursive_calls_deadlock_free && df)))
        done);
        (!error_free, !recursive_calls_deadlock_free &&
                (!one_thread_can_advance || !all_stopped_threads_are_values))

    let error_and_deadlock_free (tds, g) =
        locs_table := Array.init (Array.length tds) (fun n ->
            let (e, s) = Array.get tds n in
            union (ProgImp.ThrImp.ExpImp.locations_accessed e)
                  (ProgImp.ThrImp.StoreImp.globals_stored s));
        Hashtbl.reset ht;
        check (tds, g) []
  end

module SPORSleepChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let write_error_traces = true
    let max_depth = ref 0
    let calls = ref 0
    (* See p.73 of Godefroid 1995 thesis *)
    let hsleep = Hashtbl.create 5000
    let locs_table = ref (Array.make 0 [])

    let persistent_set (tds, g) =
        let sbset xs ys = List.for_all (fun x -> List.mem x ys) xs in
        let eq xs ys = sbset xs ys && sbset ys xs in
        let locs_accsd n = Array.get !locs_table n in
        let is_enabled n =
            let (e, s) = Array.get tds n in
            match ProgImp.ThrImp.next_transition (e, s, g) with
            Some (_, b) -> b
          | None -> false in
        let enabled_index =
            let i = ref 0 in
            while not (is_enabled !i) && !i < Array.length tds -1
            do i := !i + 1 done;
            !i in
        let old_pers = ref [] in
        let pers = ref [enabled_index] in
        let locs = ref (locs_accsd 0) in
        while not (eq !pers !old_pers) do
            old_pers := !pers;
            for i = 0 to Array.length tds - 1 do
                if not (List.mem i !pers) then
                if List.exists (fun l -> List.mem l !locs) (locs_accsd i)
                then (locs := union (locs_accsd i) !locs; pers := i::!pers)
            done
        done;
        if List.for_all is_enabled !pers then !pers else
            let rec ntoz n = if n < 0 then [] else n::(ntoz (n-1)) in
            ntoz (Array.length tds - 1)

    (* True iff error-free *)
    let rec check init_prog t_seq sleep_set =
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;

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

        let persists = persistent_set (tds, g) in

        (* (G95) for all t in T do *)
        for i = 0 to Array.length tds - 1 do
            if List.mem i !to_explore && List.mem i persists then (
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              (* No futher transitions: check if this thread reaches a local error *)
              None -> let (reaches_err, reaches_nonval) =
                        ProgImp.ThrImp.check_local (e, s, g) in
                      if reaches_err then (
                        error_free := false;
                        ProgImp.output_hasse_image "errortrace.gv" t_seq
                      );
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
                        if not (next_t'.ProgImp.ThrImp.g_loc =
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
        (!error_free, !recursive_calls_deadlock_free && (!one_thread_can_advance ||
            List.length sleep_set > 0 || !all_stopped_threads_are_values))

    let error_and_deadlock_free (tds, g) =
        locs_table := Array.init (Array.length tds) (fun n ->
            let (e, s) = Array.get tds n in
            union (ProgImp.ThrImp.ExpImp.locations_accessed e)
                  (ProgImp.ThrImp.StoreImp.globals_stored s));
        Hashtbl.reset hsleep;
        check (tds, g) [] []
  end

module SPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SPORChecker (Program.PLProgram)

module SPORSleepPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SPORSleepChecker (Program.PLProgram)
