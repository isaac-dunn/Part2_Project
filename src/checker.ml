module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp = Prog
    let print_debug = false

    let rec check init_prog t_seq =
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in
        let err_reached = ref false in
        for i = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              None -> ()
            | Some t_tran -> 
                if ProgImp.ThrImp.ExpImp.is_error t_tran.next_expr
                then (err_reached := true; if print_debug then print_string "ERROR\n";
                 let print_and_apply stat tran =
                  print_string (ProgImp.string_of_program stat);
                  ProgImp.apply_transition stat tran
                 in print_string (ProgImp.string_of_program (
                    List.fold_left print_and_apply init_prog (t_seq @ [(i, t_tran)]))))
                else if not !err_reached
                    then err_reached :=  check init_prog (t_seq @ [(i, t_tran)])
        done; not !err_reached
  end

module SimplePLChecker : (Interfaces.Checker
    with module ProgImp = Program.PLProgram)
        = SimpleChecker (Program.PLProgram)
