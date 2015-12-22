module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let print_debug = false
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
            if print_debug then print_endline ("Depth: " ^ (string_of_int (List.length t_seq))
                ^ " Thread: " ^ (string_of_int i)
                ^ " Expression: " ^ (ProgImp.ThrImp.ExpImp.string_of_expr e));
            match ProgImp.ThrImp.next_transition (e, s, g) with
              (* No futher transitions: check if this thread reaches a local error *)
              None ->
                let rec check_local (ex, st) =
                    if ProgImp.ThrImp.ExpImp.is_error ex
                    then (no_err_reached := false; if print_debug then
                            print_endline ("Local error reached: " ^
                                (ProgImp.ThrImp.ExpImp.string_of_expr ex)))
                    else match ProgImp.ThrImp.next_step (ex, st, g) with
                        None -> if print_debug then print_endline "No local errors" (* No errors *)
                      | Some t_step -> check_local (t_step.ProgImp.ThrImp.new_expr,
                                        (match t_step.ProgImp.ThrImp.s_update with
                                          None -> st
                                        | Some su -> ProgImp.ThrImp.StoreImp.update st su))
                in if !no_err_reached then check_local (e, s)
              (* There is a transition: check if error; explore from the new state *)
            | Some t_tran -> 
                if ProgImp.ThrImp.ExpImp.is_error t_tran.ProgImp.ThrImp.next_expr
                (* This is error; mark it as such and print debug *)
                then (no_err_reached := false; if print_debug then (print_string "ERROR\n";
                 let print_and_apply stat tran =
                  print_string (ProgImp.string_of_program stat);
                  ProgImp.apply_transition stat tran
                 in print_string (ProgImp.string_of_program (
                    List.fold_left print_and_apply init_prog (t_seq @ [(i, t_tran)])))))
                (* Not an error; explore subsequent transitions *)
                else if !no_err_reached
                    then no_err_reached := check init_prog (t_seq @ [(i, t_tran)])
        done;
        if print_debug then print_endline ("no_err_reached: "
            ^ (string_of_bool !no_err_reached)
            ^ " depth: " ^ (string_of_int (List.length t_seq)));
        !no_err_reached

    let error_free init_prog = check init_prog []
  end

module DPORChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    module T = ProgImp.ThrImp
    let print_debug = false
    let max_depth = ref 0
    let calls = ref 0

    (* Array for backtracking sets *)
    let backtracks = Var_array.empty()
    (* Array for done sets *)
    let dones = Var_array.empty()
    (* Track the one bit of info we're after *)
    let no_err_reached = ref true


    (* True iff error-free *)
    let rec check init_prog t_seq proc_cvs obj_cvs last_ti =
        (* Track execution info *)
        calls := !calls + 1;
        let depth = List.length t_seq in
        if depth > !max_depth then max_depth := depth;

        (* Used to determine if there is some enabled p *)
        let transition_to_explore = ref None in

        (* let s = last(S) *)
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in

        (* for all processes p *)
        for p = 0 to Array.length tds - 1 do

            let (e, s) = Array.get tds i in
            if print_debug then print_endline ("Depth: " ^ (string_of_int !depth)
                ^ " Thread: " ^ (string_of_int i)
                ^ " Expression: " ^ (T.ExpImp.string_of_expr e));

            (* Is there next transition? *)
            match ProgImp.ThrImp.next_transition (e, s, g) with

              (* No futher transitions: check if this thread reaches a local error *)
              None ->
                let rec check_local (ex, st) =
                    if T.ExpImp.is_error ex
                    then (no_err_reached := false; if print_debug then
                            print_endline ("Local error reached: " ^
                                (T.ExpImp.string_of_expr ex)))
                    else match T.next_step (ex, st, g) with
                        None -> if print_debug then print_endline "No local errors" (* No errors *)
                      | Some t_step -> check_local (t_step.T.new_expr,
                                        (match t_step.T.s_update with
                                          None -> st
                                        | Some su -> T.StoreImp.update st su))
                in if !no_err_reached then check_local (e, s)

              (* There is a transition: check if error  *)
            | Some next_t -> 
                (* There is at least one transition to explore: this one *)
                transition_to_explore := (p, next_t);
                if T.ExpImp.is_error next_t.T.next_expr (* TODO determine if necessary *)
                (* This is error; mark it as such and print debug *)
                then (no_err_reached := false; if print_debug then (print_string "ERROR\n";
                 let print_and_apply stat tran =
                  print_string (ProgImp.string_of_program stat);
                  ProgImp.apply_transition stat tran
                 in print_string (ProgImp.string_of_program (
                    List.fold_left print_and_apply init_prog (t_seq @ [(i, next_t)])))))
                (* Not an error; explore subsequent transitions *)
                else
                  (* let i = L(alpha(next(s,p))) *)
                  let i = last_ti next_t.T.g_loc in

                    (* if i =/= 0 and not i <= C(p)(proc(Si)) *)
                    if i > 0 and
                       i > ClockVector.get (proc_cvs p) (fst (List.nth t_seq i))
                    then (
                        (* if p in enabled(pre(S, i)) *)
                        (* then add p to backtrack(pre(S, i)) *)
                        Var_array.set backtracks i (p::(Var_array.get backtracks i))
                        (* else add enabled(pre(S,i)) to backtrack(pre(S,i)) *)
                        (* As we have no locks, all transitions are always enabled *)
                    )
        done;

        (* if there is some p in enabled(s) *)
        (* As we have no locks, all available transitions are enabled *)
        match !transition_to_explore with None -> () | Some (p, next_t) ->

        (* backtrack(s) := {p} *)
        Var_array.append backtracks [p]; (* TODO remove_last at end *)

        (* let S' = S.next(s,p) *)
        let new_t_seq = t_seq @ [next_t] in

        (* let o = alpha(next(s,p)) *) 
        let o = next_t.g_loc in

        (* let cv = max(C(p),C(o))[p:=|S'|] *)
        let cv = Clockvector.set (Clockvector.max (proc_cvs p) (obj_cvs o) ) p (List.length new_t_seq) in

        (* let C' = C[p:=cv, o:=cv] *)
        let new_proc_cvs pi = if pi = p then cv else proc_cvs pi in
        let new_obj_cvs oi = if oi = o then cv else obj_cvs oi in

        (* let L' = L[o:=|S'|] *)
        let new_last_ti oi = if oi = o then List.length new_t_seq else last_ti oi in

        (* Explore(S', C', L') *)
        check init_prog new_t_seq new_proc_cvs new_obj_cvs new_last_ti;
 
        if print_debug then print_endline ("no_err_reached: "
            ^ (string_of_bool !no_err_reached)
            ^ " depth: " ^ (string_of_int (List.length t_seq)));
        !no_err_reached
  end

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleChecker (Program.PLProgram)
