(* *)
(* Isaac Dunn 17/12/2015 *)

(* A thread step is a new expression, optional updates to the stores, and
* which global location, if any, was accessed *)
type thread_step = expr * ((loc * expr) option) * ((loc * expr) option) * (loc option);;

(* A transition is a new expression, store updates for local and global stores, and which
* global location was accessed *)
type thread_transition = expr * store * store * loc;;

(* next : (expr * store * store) -> thread_step option *)
(* Given expression, local store, global store, gives next thread step if it exists *)
let rec next (e, s, g)  = match e with
    Integer _ -> None
  | Boolean _ -> None
  | Op (Integer n, Plus, Integer m) -> Some (Integer (n+m), None, None, None)
  | Op (Integer n, Minus, Integer m) -> Some (Integer (n-m), None, None, None)
  | Op (Integer n, Mult, Integer m) -> Some (Integer (n*m), None, None, None)
  | Op (Integer n, Div, Integer m) -> Some (Integer (n/m), None, None, None)
  | Op (Integer n, Mod, Integer m) -> Some (Integer (n mod m), None, None, None)
  | Op (Integer n, GT, Integer m) -> Some (Boolean (n > m), None, None, None)
  | Op (Integer n, Equals, Integer m) -> Some (Boolean (n = m), None, None, None)
  | Op (e1, op, e2) -> (if is_value e1 then
                        match next (e2, s, g) with
                            Some (f, t, h, lo) -> Some (Op (e1, op, f), t, h, lo)
                          | None -> None
                       else
                        match next (e1, s, g) with
                            Some (f, t, h, lo) -> Some (Op (f, op, e2), t, h, lo)
                          | None -> None)
  | If (e1, e2, e3) -> (match e1 with
                        Boolean b -> if b then Some (e2, None, None, None) else Some (e3, None, None, None)
                      | _ -> match next (e1, s, g) with
                            Some (f, t, h, lo) -> Some (If (f, e2, e3), t, h, lo)
                          | None -> None)
  | Assign (Loc l, e2) -> if is_value e2 then Some (Skip, Some (l, e2), None, None) else
                            (match next (e2, s, g) with
                                Some (f, t, h, lo) -> Some (Assign (Loc l, f), t, h, lo)
                              | None -> None)
  | Assign (e1, e2) -> (match next (e1, s, g) with (* e1 not a location so reduce *)
                        Some (f, t, h, lo) -> Some (Assign (f, e2), t, h, lo)
                      | None -> None)
  | Deref (Loc l) -> (match get s l with
                        Some v -> Some (v, None, None, None)
                      | None -> None)
  | Deref (Glo l) -> (match get g l with
                        Some v -> Some (v, None, None, Some l)
                      | None -> None)
  | Deref e1 -> (match next (e1, s, g) with
                    Some (f, t, h, lo) -> Some (Deref f, t, h, lo)
                  | None -> None)
  | Ref e1 -> if is_value e1 then
                    let fl = get_fresh_loc s in Some (Loc fl, Some (fl, e1), None, None)
                else (match next (e1, s, g) with
                        Some (f, t, h, lo) -> Some (Ref f, t, h, lo)
                      | None -> None)
  | Loc _ -> None
  | Glo _ -> None
  | Skip -> None
  | Seq (Skip, e2) -> Some (e2, None, None, None)
  | Seq (e1, e2) -> (match next (e1, s, g) with
                        Some (f, t, h, lo) -> Some (Seq (f, e2), t, h, lo)
                      | None -> None)
  | While (e1, e2) -> Some (If (e1, Seq (e2, While (e1, e2)), Skip), None, None, None)
  | Fn (_, _) -> None
  | App (Fn (t1, e1), e2) -> if is_value e2 then
                            Some (subst e2 0 e1, None, None, None)
                        else (match next (e2, s, g) with
                            Some (f, t, h, lo) -> Some (App (Fn (t1, e1), f), t, h, lo)
                          | None -> None)
  | App (e1, e2) -> (match next (e1, s, g) with
                        Some (f, t, h, lo) -> Some (App (f, e2), t, h, lo)
                      | None -> None)
  | Var _ -> None
  | Let (t1, e1, e2) -> if is_value e1 then Some (subst e1 0 e2, None, None, None)
                        else (match next (e1, s, g) with
                            Some (f, t, h, lo) -> Some (Let (t1, f, e2), t, h, lo)
                          | None -> None)
  | Letrec (t1, t2, e1, e2) -> (* Need to adjust de Bruijn indices as new binding contexts for e1 *)
      Some (subst (Fn (t1, Letrec (t1, t2, shift 2 e1, swap 0 e1))) 0 e2, None, None, None)
  | Cas (Glo l, e2, e3) -> if is_value e2 then (* Reduce e1 then e2 then e3 to values *)
                            (if is_value e3 then (match get g l with
                                Some v -> if v = e2 then Some (Boolean true, None, Some (l, e3), Some l)
                                             else Some (Boolean false, None, None, Some l)
                              | None -> None)
                            else (match next (e3, s, g) with
                             Some (f, t, h, lo) -> Some (Cas (Glo l, e2, f), t, h, lo)
                           | None -> None))
                           else (match next (e2, s, g) with
                            Some (f, t, h, lo) -> Some (Cas (Glo l, f, e3), t, h, lo)
                          | None -> None)
  | Cas (e1, e2, e3) -> (match next (e1, s, g) with
                            Some (f, t, h, lo) -> Some (Cas (f, e2, e3), t, h, lo)
                          | None -> None)
  | Error msg -> None;;

(* next_transition : expr * store * store -> (expr * store * store * loc) option *)
(* Given expression, local state and global state, gives new expression
* local store update, global store update and global location touched *)
let rec next_transition (e, s, g) =
    let extract opt = match opt with
        Some su -> [su] | None -> [] in
    let extend sto sopt = match sopt with
        Some su -> su::sto | None -> sto in
    match next (e, s, g) with
        Some (f, t, h, lo) -> (match lo with
            Some l -> Some (f, extract t, extract h, l)
          | None -> (match next_transition (f, extend s t, g) with
                Some (e_res, s_res, g_res, l_res) -> Some (e_res, extend s_res t, g_res, l_res)
              | None -> None))
      | None -> None;;

(* evaluate : expre * store * store -> expr * store * store *)
(* Run one thread to completion in isolation *)
let rec evaluate (e, s, g) = 
    let extend sto opt = match opt with Some u -> u::sto | None -> sto in
    match next (e, s, g) with
        Some (f, t, h, _) -> evaluate (f, extend s t, extend g h)
      | None -> (e, s, g)

let rec print_exec(e, s, g) = pretty_print(e); print_newline(); print_newline();  match next (e, s, g) with
    Some (f, t, h, l) -> (match t with None -> (match h with None -> print_exec (f, s, g)
                                                       | Some gu -> print_exec (f, s, gu::g))
                                 | Some su -> (match h with None -> print_exec (f, su::s, g)
                                                         | Some gu -> print_exec (f, su::s, gu::g)))
  | None -> print_string "END\n";;

type thread_state = expr * store;;

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
    
