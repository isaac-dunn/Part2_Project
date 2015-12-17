(* PL Threads *)
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

let rec string_of_execution (e, s, g) = (string_of_exp e) ^ "\n\n" ^
    (match next (e, s, g) with
      Some (f, t, h, l) -> (match t with
            None -> (match h with None -> string_of_execution (f, s, g)
                                | Some gu -> string_of_execution (f, s, gu::g))
          | Some su -> (match h with None -> string_of_execution (f, su::s, g)
                                   | Some gu -> string_of_execution (f, su::s, gu::g)))
    | None -> "END\n";;

