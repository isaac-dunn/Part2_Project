(* *)
(* Isaac Dunn 17/12/2015 *)

(* Checks program state for errors and deadlocks *)
let rec explore initial ts =
    let (tds, g) = apply_thread_steps (List.rev ts) initial in
    let exists_transition = ref false in
    for i = 0 to Array.length tds - 1 do
        let (e, s) = Array.get tds i in
        match next (e, s, g) with
            None -> ()
          | Some (f, t, h, l) -> exists_transition := true; if is_error f then (print_string "ERROR\n"; print_thread_steps initial (List.rev ((i, f, t, h) :: ts)))
                              else explore initial ((i, f, t, h) :: ts)
    done; if !exists_transition then ()
          else (print_string "DEADLOCK\n"; print_thread_steps initial (List.rev ts));;

(* Checks program state for errors and sticks at non-values *)
let rec check_program prog transitions =
    let (threads, g) = apply_thread_steps (List.rev transitions) prog in
    let error = ref false in
    let is_stuck = ref true in
    let sticks_at_nonvalue = ref false in
    for i = 0 to Array.length threads - 1 do
        let(e, s) = Array.get threads i in
        match next (e, s, g) with
            None -> sticks_at_nonvalue := !sticks_at_nonvalue || not (is_value e)
          | Some (f, t, h, l) -> let (err, sanv) = check_program prog ((i, f, t, h) :: transitions) in
                                error := !error || is_error f || err; is_stuck := false;
                                sticks_at_nonvalue := !sticks_at_nonvalue || sanv
    done; (!error, !is_stuck);;
    
