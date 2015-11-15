(* Language for Part II Project *)
(* Isaac Dunn 8/11/2015 *)

open Random;;

(*** SYNTAX ***)

type type_expr =
     Int
   | Unit
   | Bool
   | Ref of type_expr
   | Func of type_expr * type_expr;;

let rec print_type t = match t with
    Int -> print_string "int"
  | Unit -> print_string "unit"
  | Bool -> print_string "bool"
  | Ref t -> print_type t; print_string " ref"
  | Func (t1, t2) -> print_type t1; print_string " -> "; print_type t2;;

type loc = string;;

type oper =
     Plus
   | Minus
   | Mult
   | Div
   | Mod
   | GT
   | Equals;;

let print_op o = match o with
     Plus -> print_string "+"
   | Minus -> print_string "-"
   | Mult -> print_string "*"
   | Div -> print_string "/"
   | Mod -> print_string "mod"
   | GT -> print_string ">"
   | Equals -> print_string "=";;

type var_raw = string;;

(* raw expressions *)
type expr_raw =
     Integer_raw of int
   | Boolean_raw of bool
   | Op_raw of expr_raw * oper * expr_raw
   | If_raw of expr_raw * expr_raw * expr_raw
   | Assign_raw of expr_raw * expr_raw
   | Deref_raw of expr_raw
   | Ref_raw of expr_raw
   | Glo_raw of loc
   | Skip_raw
   | Seq_raw of expr_raw * expr_raw
   | While_raw of expr_raw * expr_raw
   | Var_raw of var_raw
   | Fn_raw of var_raw * type_expr * expr_raw
   | App_raw of expr_raw * expr_raw
   | Let_raw of var_raw * type_expr * expr_raw * expr_raw
   | Letrec_raw of var_raw * type_expr * var_raw * type_expr * expr_raw * expr_raw
   | Cas_raw of expr_raw * expr_raw * expr_raw
   | Error_raw of string;;

(* expressions up to alpha equivalence *)
type expr = 
     Integer of int
   | Boolean of bool
   | Op of expr * oper * expr
   | If of expr * expr * expr
   | Assign of expr * expr
   | Deref of expr
   | Ref of expr
   | Loc of loc
   | Glo of loc
   | Skip
   | Seq of expr * expr
   | While of expr * expr
   | Var of int
   | Fn of type_expr * expr
   | App of expr * expr
   | Let of type_expr * expr * expr
   | Letrec of type_expr * type_expr * expr * expr
   | Cas of expr * expr * expr
   | Error of string;;

let rec pretty_print e = match e with
    Integer n -> print_int n
  | Boolean true -> print_string "true"
  | Boolean false -> print_string "false"
  | Op (e1, op, e2) -> pretty_print e1; print_string " ";
                       print_op op; print_string " "; pretty_print e2
  | If (e1, e2, e3) -> print_string "if "; pretty_print e1;
                       print_string " then "; pretty_print e2;
                       print_string " else "; pretty_print e3
  | Assign (e1, e2) -> pretty_print e1; print_string " := "; pretty_print e2
  | Deref e1 -> print_string "!("; pretty_print e1; print_string ")"
  | Ref e1 -> print_string "ref ("; pretty_print e1; print_string ")"
  | Loc l -> print_string ("Local:"^l)
  | Glo l -> print_string ("Global:"^l)
  | Skip -> print_string "skip"
  | Seq (e1, e2) -> pretty_print e1; print_string "; "; pretty_print e2
  | While (e1, e2) -> print_string "while "; pretty_print e1; print_string " do ";
                        pretty_print e2; print_string " done"
  | Var n -> print_string "V:"; print_int n
  | Fn (t, e1) -> print_string "fn O : "; print_type t; print_string " => "; pretty_print e1
  | App (e1, e2) -> print_string "("; pretty_print e1; print_string ") ("; pretty_print e2; print_string ")"
  | Let (t1, e1, e2) -> print_string "let val O : "; print_type t1; print_string " = ";
                         pretty_print e1; print_string " in "; pretty_print e2; print_string " end"
  | Letrec (t1, t2, e1, e2) -> print_string "let rec O : "; print_type t1; print_string " -> ";
                                print_type t2; print_string " = (fn O : "; print_type t1;
                                print_string " => "; pretty_print e1; print_string ") in ";
                                pretty_print e2; print_string " end"
  | Cas (e1, e2, e3) -> print_string "CAS("; pretty_print e1; print_string ", ";
                        pretty_print e2; print_string ", "; pretty_print e3; print_string ")"
  | Error msg -> print_string ("Error: "^msg);;

(*** SEMANTICS ***)

(* find_first : var_raw -> var_raw list -> int -> int option *)
(* find_first x ys 0 gives index of first occurance of x in ys *)
let rec find_first x zs m = match zs with
    [] -> None
  | y::ys -> if y=x then Some m else find_first x ys (m+1);;

exception Resolve of string;;

(* resolve : var_raw list -> expr_raw -> expr *)
(* Given list of bound vars and raw expr. gives an de Bruijn expr.*)
let rec resolve env expr_raw = match expr_raw with
    Integer_raw n -> Integer n
  | Boolean_raw b -> Boolean b
  | Op_raw (e1, op, e2) -> Op (resolve env e1, op, resolve env e2)
  | If_raw (e1, e2, e3) -> If (resolve env e1,
                               resolve env e2,
                               resolve env e3)
  | Assign_raw (e1, e2) -> Assign (resolve env e1, resolve env e2)
  | Deref_raw e -> Deref (resolve env e)
  | Ref_raw e -> Ref (resolve env e)
  | Glo_raw l -> Glo l
  | Skip_raw -> Skip
  | Seq_raw (e1, e2) -> Seq (resolve env e1, resolve env e2)
  | While_raw (e1, e2) -> While(resolve env e1, resolve env e2)
  | Var_raw x -> (match find_first x env 0 with
                    None -> raise (Resolve ("Bogus unbound var_raw: " ^ x))
                  | Some n -> Var n)
  | Fn_raw (x, t, e) -> Fn (t, resolve (x::env) e)
  | App_raw (e1, e2) -> App (resolve env e1, resolve env e2)
  | Let_raw (x, t, e1, e2) -> Let (t, resolve env e1, resolve (x::env) e2)
  | Letrec_raw (x, tx, y, ty, e1, e2) ->
        Letrec (tx, ty, resolve (y::x::env) e1, resolve (x::env) e2)
  | Cas_raw (e1, e2, e3) -> Cas (resolve env e1,
                                 resolve env e2,
                                 resolve env e3)
  | Error_raw msg -> Error msg;;

let is_error e = match e with
    Error _ -> true
  | _ -> false;;

(* is_value : expr -> bool *)
let is_value e = match e with
    Integer _ -> true
  | Boolean _ -> true
  | Loc _ -> true
  | Glo _ -> true
  | Skip -> true
  | Error _ -> true
  | Fn (_, _) -> true
  | _ -> false;;

(* Stores are finite partial maps from locations to values *)
type store = (loc * expr) list;;

let rec member x xs = match xs with
    [] -> false
  | y::ys -> if x=y then true else member x ys;;

let print_store s = 
    let rec work_through left seen = match left with
        [] -> ()
      | (l, e)::es -> (if member l seen then () else (print_string (l^": "); pretty_print e; print_string "; "));
                        work_through es (l::seen)
    in work_through s [];;

let rec get s l = match s with
    [] -> None
  | (o, e)::rest -> if o=l then Some e else get rest l;;

(* update : store -> loc -> expr -> store *)
(* Gives store with given location updated to new value *)
let update s l v = (l, v)::s;;

(* subst : expr -> int -> expr -> expr *)
(* subst e 0 f gives f with e substituted for the outmost variable *)
let rec subst e n f = match f with
    Integer m -> Integer m
  | Boolean b -> Boolean b
  | Op (e1, op, e2) -> Op (subst e n e1, op, subst e n e2)
  | If (e1, e2, e3) -> If (subst e n e1,
                           subst e n e2,
                           subst e n e3)
  | Assign (e1, e2) -> Assign (subst e n e1, subst e n e2)
  | Deref e1 -> Deref (subst e n e1)
  | Ref e1 -> Ref (subst e n e1)
  | Loc l -> Loc l
  | Glo l -> Glo l
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (subst e n e1, subst e n e2)
  | While (e1, e2) -> While (subst e n e1, subst e n e2)
  | Var m -> if n=m then e else Var m
  | Fn (t, e1) -> Fn (t, subst e (n+1) e1)
  | App (e1, e2) -> App (subst e n e1, subst e n e2)
  | Let (t, e1, e2) -> Let (t, subst e n e1, subst e (n+1) e2)
  | Letrec (t1, t2, e1, e2) -> Letrec (t1, t2, subst e (n+2) e1, subst e (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (subst e n e1,
                             subst e n e2,
                             subst e n e3)
  | Error msg -> Error msg;;

(* shift n e increments (by 1) all the variable indices >=n in e *)
let rec shift n e = match e with
    Integer m  -> Integer m
  | Boolean b -> Boolean b
  | Op (e1,opr,e2) -> Op (shift n e1, opr, shift n e2)
  | If (e1,e2,e3) -> If (shift n e1, shift n e2, shift n e3)
  | Assign (e1, e2) -> Assign (shift n e1, shift n e2)
  | Deref e1 -> Deref (shift n e1)
  | Ref e1 -> Ref (shift n e1)
  | Loc l -> Loc l
  | Glo l -> Glo l
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (shift n e1, shift n e2)
  | While (e1, e2) -> While (shift n e1, shift n e2)
  | Var m -> if m >= n then Var (m+1) else Var m
  | Fn (t, e1) -> Fn (t, shift (n+1) e1) 
  | App (e1, e2) -> App (shift n e1, shift n e2)
  | Let (t, e1, e2) -> Let (t, shift n e1, shift (n+1) e2)
  | Letrec (t1, t2, e1, e2) -> Letrec (t1, t2, shift (n+2) e1, shift (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (shift n e1, shift n e2, shift n e3)
  | Error msg -> Error msg;;


(* swap n e swaps the nth and n+1th variable indices in e *)
let rec swap n e = match e with
    Integer m -> Integer m
  | Boolean b -> Boolean b
  | Op (e1, op, e2) -> Op (swap n e1, op, swap n e2)
  | If (e1, e2, e3) -> If (swap n e1, swap n e2, swap n e3)
  | Assign (e1, e2) -> Assign (swap n e1, swap n e2)
  | Deref e1 -> Deref (swap n e1)
  | Ref e1 -> Ref (swap n e1)
  | Loc l -> Loc l
  | Glo l -> Glo l
  | Skip -> Skip
  | Seq (e1, e2) -> Seq (swap n e1, swap n e2)
  | While (e1, e2) -> While (e1, e2)
  | Var m -> if m=n then Var (n+1) else if m=n+1 then Var n else Var m
  | Fn (t, e1) -> Fn (t, swap (n+1) e1)
  | App (e1, e2) -> App (swap n e1, swap n e2)
  | Let (t, e1, e2) -> Let (t, swap n e1, swap (n+1) e2)
  | Letrec (t1, t2, e1, e2) -> Letrec (t1, t2, swap (n+2) e1, swap (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (swap n e1, swap n e2, swap n e3)
  | Error msg -> Error msg;;

(* get_fresh_loc : store -> loc *)
(* Gives a location unused in the given store *)
let get_fresh_loc s = let fl = ref "L0" in while (get s !fl != None) do
                        fl := !fl ^ string_of_int(Random.int 10) done; !fl;;

(* A transition is a new expression, and an optional updates for local and global stores *)
type thread_transition = expr * ((loc * expr) option) * ((loc * expr) option);;

(* next : (expr * store * store) -> thread_transitoin option *)
(* Given expression, local store, global store, gives next expression if it exists *)
let rec next (e, s, g)  = match e with
    Integer _ -> None
  | Boolean _ -> None
  | Op (Integer n, Plus, Integer m) -> Some (Integer (n+m), None, None)
  | Op (Integer n, Minus, Integer m) -> Some (Integer (n-m), None, None)
  | Op (Integer n, Mult, Integer m) -> Some (Integer (n*m), None, None)
  | Op (Integer n, Div, Integer m) -> Some (Integer (n/m), None, None)
  | Op (Integer n, Mod, Integer m) -> Some (Integer (n mod m), None, None)
  | Op (Integer n, GT, Integer m) -> Some (Boolean (n > m), None, None)
  | Op (Integer n, Equals, Integer m) -> Some (Boolean (n = m), None, None)
  | Op (e1, op, e2) -> (if is_value e1 then
                        match next (e2, s, g) with
                            Some (f, t, h) -> Some (Op (e1, op, f), t, h)
                          | None -> None
                       else
                        match next (e1, s, g) with
                            Some (f, t, h) -> Some (Op (f, op, e2), t, h)
                          | None -> None)
  | If (e1, e2, e3) -> (match e1 with
                        Boolean b -> if b then Some (e2, None, None) else Some (e3, None, None)
                      | _ -> match next (e1, s, g) with
                            Some (f, t, h) -> Some (If (f, e2, e3), t, h)
                          | None -> None)
  | Assign (Loc l, e2) -> if is_value e2 then Some (Skip, Some (l, e2), None) else
                            (match next (e2, s, g) with
                                Some (f, t, h) -> Some (Assign (Loc l, f), t, h)
                              | None -> None)
  | Assign (e1, e2) -> (match next (e1, s, g) with (* e1 not a location so reduce *)
                        Some (f, t, h) -> Some (Assign (f, e2), t, h)
                      | None -> None)
  | Deref (Loc l) -> (match get s l with
                        Some v -> Some (v, None, None)
                      | None -> None)
  | Deref e1 -> (match next (e1, s, g) with
                    Some (f, t, h) -> Some (Deref f, t, h)
                  | None -> None)
  | Ref e1 -> if is_value e1 then
                    let fl = get_fresh_loc s in Some (Loc fl, Some (fl, e1), None)
                else (match next (e1, s, g) with
                        Some (f, t, h) -> Some (Ref f, t, h)
                      | None -> None)
  | Loc _ -> None
  | Glo _ -> None
  | Skip -> None
  | Seq (Skip, e2) -> Some (e2, None, None)
  | Seq (e1, e2) -> (match next (e1, s, g) with
                        Some (f, t, h) -> Some (Seq (f, e2), t, h)
                      | None -> None)
  | While (e1, e2) -> Some (If (e1, Seq (e2, While (e1, e2)), Skip), None, None)
  | Fn (_, _) -> None
  | App (Fn (t1, e1), e2) -> if is_value e2 then
                            Some (subst e2 0 e1, None, None)
                        else (match next (e2, s, g) with
                            Some (f, t, h) -> Some (App (Fn (t1, e1), f), t, h)
                          | None -> None)
  | App (e1, e2) -> (match next (e1, s, g) with
                        Some (f, t, h) -> Some (App (f, e2), t, h)
                      | None -> None)
  | Var _ -> None
  | Let (t1, e1, e2) -> if is_value e1 then Some (subst e1 0 e2, None, None)
                        else (match next (e1, s, g) with
                            Some (f, t, h) -> Some (Let (t1, f, e2), t, h)
                          | None -> None)
  | Letrec (t1, t2, e1, e2) -> (* Need to adjust de Bruijn indices as new binding contexts for e1 *)
      Some (subst (Fn (t1, Letrec (t1, t2, shift 2 e1, swap 0 e1))) 0 e2, None, None)
  | Cas (Glo l, e2, e3) -> if is_value e2 then (* Reduce e1 then e2 then e3 to values *)
                            (if is_value e3 then (match get g l with
                                Some v -> if v = e2 then Some (Boolean true, None, Some (l, e3))
                                             else Some (Boolean false, None, None)
                              | None -> None)
                            else (match next (e3, s, g) with
                             Some (f, t, h) -> Some (Cas (Glo l, e2, f), t, h)
                           | None -> None))
                           else (match next (e2, s, g) with
                            Some (f, t, h) -> Some (Cas (Glo l, f, e3), t, h)
                          | None -> None)
  | Cas (e1, e2, e3) -> (match next (e1, s, g) with
                            Some (f, t, h) -> Some (Cas (f, e2, e3), t, h)
                          | None -> None)
  | Error msg -> None;;

let rec evaluate (e, s, g) = match next (e, s, g) with
    Some (f, t, h) -> (match t with None -> (match h with None -> evaluate (f, s, g)
                                                       | Some gu -> evaluate (f, s, gu::g))
                                 | Some su -> (match h with None -> evaluate (f, su::s, g)
                                                         | Some gu -> evaluate (f, su::s, gu::g)))
  | None -> (e, s, g);;

let rec print_exec(e, s, g) = pretty_print(e); print_newline(); print_newline();  match next (e, s, g) with
    Some (f, t, h) -> (match t with None -> (match h with None -> print_exec (f, s, g)
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

let apply_transition (i, e, su, gu) (tds, g) =
    let new_tds = Array.copy tds in
    let (old_e, old_s) = Array.get tds i in
        (match su with None -> Array.set new_tds i (e, old_s)
                    | Some news -> Array.set new_tds i (e, news::old_s));
        (match gu with None -> (new_tds, g)
                     | Some news -> (new_tds, news::g));;

let rec apply_transitions ts ps = match ts with
    [] -> ps
  | t :: rest -> apply_transitions rest (apply_transition t ps);;

let rec print_transitions initial ts = print_prog_state initial; print_newline(); match ts with
    [] -> ()
  | u::us -> print_transitions (apply_transition u initial) us;;

let rec explore initial ts =
    let (tds, g) = apply_transitions (List.rev ts) initial in
    let exists_transition = ref false in
    for i = 0 to Array.length tds - 1 do
        let (e, s) = Array.get tds i in
        match next (e, s, g) with
            None -> ()
          | Some (f, t, h) -> exists_transition := true; if is_error f then (print_string "ERROR\n"; print_transitions initial (List.rev ((i, f, t, h) :: ts)))
                              else explore initial ((i, f, t, h) :: ts)
    done; if !exists_transition then ()
          else (print_string "DEADLOCK\n"; print_transitions initial (List.rev ts));;

let rec error_reachable initial ts =
    let (tds, g) = apply_transitions (List.rev ts) initial in
    let error = ref false in
    for i = 0 to Array.length tds - 1 do
        let (e, s) = Array.get tds i in
        match next (e, s, g) with
            None -> ()
          | Some (f, t, h) -> error := is_error f || error_reachable initial ((i, f, t, h) :: ts)
    done; !error;;
    
let tds = Array.make 2 (Skip, []);;
    Array.set tds 0 (If (Cas(Glo "x", Integer 2, Integer 0), Integer 100, Error "Did not get CAS"), []);
    Array.set tds 1 (Cas(Glo "x", Integer 2, Integer 1), []);
    print_string (string_of_bool (error_reachable (tds, [("x", Integer 2)]) [])); print_newline();;


