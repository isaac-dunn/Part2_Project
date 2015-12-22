module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let print_debug = false

    (* True iff error-free *)
    let rec check init_prog t_seq =
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
  end

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleChecker (Program.PLProgram)
