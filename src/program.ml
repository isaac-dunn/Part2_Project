(* PL Program *)
(* Isaac Dunn 17/12/2015 *)

module Program (Thr : Interfaces.Thread) = struct
    module ThrImp : Interfaces.Thread = Thr

    (* A program consists of threads and a global store *)
    type state = (ThrImp.ExpImp.expr * ThrImp.StoreImp.store) array * ThrImp.StoreImp.store

    (* A thread step with the index of the thread *)
    type step = int * ThrImp.step
    (* A thread transition with the index of the thread *)
    type transition = int * ThrImp.transition

    let string_of_program (tds, g) =
        let acc = ref "" in
        for i = 0 to Array.length tds - 1 do
            let (e, s) = Array.get tds i in
                acc := !acc ^ "Thread " ^ (string_of_int i) ^ "\n"
                            ^ (ThrImp.ExpImp.string_of_expr e) ^ "\n"
                            ^ (ThrImp.StoreImp.string_of_store s) ^ "\n"
        done; !acc ^ "Global Store\n" ^ (ThrImp.StoreImp.string_of_store g) ^ "\n"

    let apply_step (tds, g) (i, t_step) =
        let new_tds = Array.copy tds in
        let (old_e, old_s) = Array.get tds i in
            (match t_step.ThrImp.s_update with None -> Array.set new_tds i (t_step.ThrImp.new_expr, old_s)
                        | Some news -> Array.set new_tds i (t_step.ThrImp.new_expr, ThrImp.StoreImp.update old_s news));
            (match t_step.ThrImp.g_update with None -> (new_tds, g)
                         | Some news -> (new_tds, ThrImp.StoreImp.update g news));;

    let apply_transition (tds, g) (i, t_tran) =
        let new_tds = Array.copy tds in
        let (_, old_s) = Array.get tds i in
            Array.set new_tds i (t_tran.ThrImp.next_expr, ThrImp.StoreImp.extend old_s t_tran.ThrImp.s_updates);    
            (new_tds, ThrImp.StoreImp.extend g t_tran.ThrImp.g_updates)
end

module PLProgram : Interfaces.Program = Program (Pl_thread)

