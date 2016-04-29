module Program (Thr : Interfaces.Thread) = struct
    module ThrImp = Thr

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

    (* Get edges in Hasse diagram for happens-before (graph on t_seq indices) *)
    let rec get_hasse_trace t_seq =
        (* last_{proc,obj} are L:{proc,loc}->N, hasse is the result *)
        let rec aux to_explore curr_index last_proc last_obj hasse =
            match to_explore with
              [] -> hasse
            | (p, t)::ts ->
                let o = t.ThrImp.g_loc in
                let option_extend_list ou xs = match ou with
                    None -> xs | Some u -> u::xs in
                let proc_opt = if last_proc p = -1 then None
                               else Some (last_proc p, curr_index) in
                let obj_opt = if last_obj o = -1 then None
                               else Some (last_obj o, curr_index) in
                aux ts (curr_index + 1)
                  (fun i -> if i = p then curr_index else last_proc i)
                  (fun l -> if l = o then curr_index else last_obj l)
                  (option_extend_list proc_opt
                    (option_extend_list obj_opt hasse))
        in aux t_seq 0 (fun _ -> -1) (fun _ -> -1) []

    let rec output_hasse_image filename t_seq =
        let out_chan = open_out filename in
        let write_node i (p, t) =
            Printf.fprintf out_chan "%s\n" ((string_of_int i) ^ " [label=\""
                ^ (string_of_int p) ^ " : "
                ^ (ThrImp.ExpImp.string_of_loc t.ThrImp.g_loc) ^ "\"]") in
        let write_edge (i, j) =
          let message = (string_of_int i) ^ " -> " ^ (string_of_int j) in
          Printf.fprintf out_chan "  %s\n" message in
        Printf.fprintf out_chan "%s\n" "digraph Trace {";
        List.iteri write_node t_seq;
        List.iter write_edge (get_hasse_trace t_seq);
        Printf.fprintf out_chan "%s\n" "}";
        close_out out_chan
end

module PLProgram : (Interfaces.Program
    with module ThrImp = Pl_thread)
        = Program (Pl_thread)

