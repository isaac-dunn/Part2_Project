module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let max_depth = ref 0
    let calls = ref 0

    (* True iff error-free *)
    let rec check init_prog t_seq =
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in
        let no_err_reached = ref true in
        for i = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              (* No futher transitions: check if this thread reaches a local error *)
              None ->
                let rec check_local (ex, st) =
                    if ProgImp.ThrImp.ExpImp.is_error ex
                    then no_err_reached := false
                    else match ProgImp.ThrImp.next_step (ex, st, g) with
                        None -> () (* No errors *)
                      | Some t_step -> check_local (t_step.ProgImp.ThrImp.new_expr,
                                        (match t_step.ProgImp.ThrImp.s_update with
                                          None -> st
                                        | Some su -> ProgImp.ThrImp.StoreImp.update st su))
                in if !no_err_reached then check_local (e, s)
              (* There is a transition: check if error; explore from the new state *)
            | Some t_tran -> 
                if ProgImp.ThrImp.ExpImp.is_error t_tran.ProgImp.ThrImp.next_expr
                (* This is error; mark it as such *)
                then no_err_reached := false
                (* Not an error; explore subsequent transitions *)
                else no_err_reached := !no_err_reached && check init_prog (t_seq @ [(i, t_tran)])
        done;
        !no_err_reached

    let error_free init_prog = check init_prog []
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

        (* Track the one bit of info we're after *)
        let no_err_reached = ref true in

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
              None ->
                let rec check_local (ex, st) =
                    if T.ExpImp.is_error ex
                    then no_err_reached := false
                    else match T.next_step (ex, st, g) with
                        None -> () (* No errors *)
                      | Some t_step -> check_local (t_step.T.new_expr,
                                        (match t_step.T.s_update with
                                          None -> st
                                        | Some su -> T.StoreImp.update st su))
                in if !no_err_reached then check_local (e, s)

              (* There is a transition: check if error  *)
            | Some next_t -> 
                (* There is at least one transition to explore: this one *)
                transition_to_explore := Some p;
                (* let i = L(alpha(next(s,p))) *)
                let i = last_ti next_t.T.g_loc in

                (* if i =/= ~-1 and not i <= C(p)(proc(Si)) *)
                if i > ~-1 then if
                   i > Clockvector.get (proc_cvs p) (fst (List.nth t_seq i))
                then (
                    let prei = pre i in
                    let enabled p' (tds', g') =
                        let (e', s') = Array.get tds' p' in
                        match ProgImp.ThrImp.next_transition (e', s', g') with
                        Some _ -> true | None -> false
                    (* if p in enabled(pre(S, i)) *)
                    in if enabled p prei
                    (* then add p to backtrack(pre(S, i)) *)
                    then Var_array.set backtracks i (p::(Var_array.get backtracks i))
                    (* else add enabled(pre(S,i)) to backtrack(pre(S,i)) *)
                    else let rec ntoz n = if n = 0 then [0] else n::(ntoz (n-1))
                         in let en_in_prei q = enabled q prei
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
          | Some next_t -> (

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
        no_err_reached := !no_err_reached && (check init_prog new_t_seq new_proc_cvs new_obj_cvs new_last_ti);

        (* go back to top of while loop if necessary *)
        (* also contains add p to done *)
        )); whileloop (find_p (Var_array.get backtracks (Var_array.length backtracks - 1)) (p::dones)) (p::dones))
        (* also contains let done = emptyset *)
        in whileloop (find_p (Var_array.get backtracks (Var_array.length backtracks - 1)) []) [];
            Var_array.remove_last backtracks (* Decrement size of array to counter earlier increment *)));
 
        !no_err_reached

    let error_free (tds, g) =
        let n = Array.length tds in
        check (tds, g) [] (fun _ -> Clockvector.fresh n) (fun _ -> Clockvector.fresh n) (fun _ -> ~-1)
  end

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleChecker (Program.PLProgram)

module DPORPLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = DPORChecker (Program.PLProgram)
