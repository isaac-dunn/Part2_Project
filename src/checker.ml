module SimpleChecker (Prog : Interfaces.Program) =
  struct
    module ProgImp : Interfaces.Program = Prog

    let rec check init_prog t_seq =
        let (tds, g) = List.fold_left ProgImp.apply_transition init_prog t_seq in
        for i = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds i in
            match ProgImp.ThrImp.next_transition (e, s, g) with
              None -> ()
            | Some t_tran -> 
                if ProgImp.ThrImp.ExpImp.is_error t_tran.next_expr
                then (print_string "ERROR\n";
                 let print_and_apply stat tran =
                  print_string (ProgImp.string_of_program stat);
                  ProgImp.apply_transition stat tran
                 in print_string (ProgImp.string_of_program (
                    List.fold_left print_and_apply init_prog (t_seq @ [(i, t_tran)]))))
                else check init_prog (t_seq @ [(i, t_tran)])
        done
  end

module SimplePLChecker = SimpleChecker (Program.PLProgram)
