(* PL Program *)
(* Isaac Dunn 17/12/2015 *)

(* A program consists of threads and a global store *)
type state = (Expression.expr * Store.store) array * Store.store

(* A thread step with the index of the thread *)
type step = int * Thread.step
(* A thread transition with the index of the thread *)
type transition = int * Thread.transition

let string_of_program (tds, g) =
    let acc = ref "" in
    for i = 0 to Array.length tds - 1 do
        let (e, s) = Array.get tds i in
            acc := !acc ^ "Thread " ^ (string_of_int i) ^ "\n"
                        ^ (Expression.string_of_expr e) ^ "\n"
                        ^ (Store.string_of_store s) ^ "\n"
    done; !acc ^ "Global Store\n" ^ (Store.string_of_store g) ^ "\n"

let apply_step (tds, g) (i, t_step) =
    let new_tds = Array.copy tds in
    let (old_e, old_s) = Array.get tds i in
        (match t_step.Thread.s_update with None -> Array.set new_tds i (t_step.Thread.new_expr, old_s)
                    | Some news -> Array.set new_tds i (t_step.Thread.new_expr, Store.update old_s news));
        (match t_step.Thread.g_update with None -> (new_tds, g)
                     | Some news -> (new_tds, Store.update g news));;

let apply_transition (tds, g) (i, t_tran) =
    let new_tds = Array.copy tds in
    let (_, old_s) = Array.get tds i in
        Array.set new_tds i (t_tran.Thread.next_expr, Store.extend old_s t_tran.Thread.s_updates);    
        (new_tds, Store.extend g t_tran.Thread.g_updates)
