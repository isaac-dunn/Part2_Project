(* *)
(* Isaac Dunn 17/12/2015 *)

(* A program consists of threads and a global store *)
type program_state = thread_state array * store;;

let print_prog_state (tds, g) =
    for i = 0 to Array.length tds - 1 do
        print_string ("Thread " ^ (string_of_int i) ^ "\n");
        let (e, s) = Array.get tds i in
            pretty_print e;
            print_newline();
            print_store s;
            print_newline();
    done; print_string "Global Store\n"; print_store g; print_newline();;

(* apply_thread_step : (int * expr * ((loc * expr) option) * ((loc* expr) option)) -> program_state -> program_state *)
(* Given a thread step and a program state, gives the next program state *)
let apply_thread_step (i, e, su, gu) (tds, g) =
    let new_tds = Array.copy tds in
    let (old_e, old_s) = Array.get tds i in
        (match su with None -> Array.set new_tds i (e, old_s)
                    | Some news -> Array.set new_tds i (e, news::old_s));
        (match gu with None -> (new_tds, g)
                     | Some news -> (new_tds, news::g));;

let rec apply_thread_steps ts ps = match ts with
    [] -> ps
  | t :: rest -> apply_thread_steps rest (apply_thread_step t ps);;

let rec print_thread_steps initial ts = print_prog_state initial; print_newline(); match ts with
    [] -> ()
  | u::us -> print_thread_steps (apply_thread_step u initial) us;;

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
    
