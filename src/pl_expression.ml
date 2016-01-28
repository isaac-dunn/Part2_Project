(* PL Expressions *)
(* Isaac Dunn 17/12/2015 *)

(*** SYNTAX ***)

type loc = string

let string_of_loc l = l

let rand_loc_range = ref 100
let random_loc () =
    rand_loc_range := !rand_loc_range + 20;
    "RL" ^ (string_of_int (Random.int !rand_loc_range))

type oper =
     Plus
   | Minus
   | Mult
   | Div
   | Mod
   | GT
   | LT
   | Equals
   | And
   | Or

let string_of_op o = match o with
     Plus -> "+"
   | Minus -> "-"
   | Mult -> "*"
   | Div -> "/"
   | Mod -> "mod"
   | GT -> ">"
   | LT -> "<"
   | Equals -> "="
   | And -> "&"
   | Or -> "|"

type var_raw = string

type expr_raw =
     Integer_raw of int
   | Boolean_raw of bool
   | Not_raw of expr_raw
   | Op_raw of expr_raw * oper * expr_raw
   | If_raw of expr_raw * expr_raw * expr_raw
   | Assign_raw of expr_raw * expr_raw
   | Deref_raw of expr_raw
   | Ref_raw of expr_raw
   | Loc_raw of loc
   | Glo_raw of loc
   | Skip_raw
   | Seq_raw of expr_raw * expr_raw
   | While_raw of expr_raw * expr_raw
   | Var_raw of var_raw
   | Fn_raw of var_raw * expr_raw
   | App_raw of expr_raw * expr_raw
   | Let_raw of var_raw * expr_raw * expr_raw
   | Letrec_raw of var_raw * var_raw * expr_raw * expr_raw
   | Cas_raw of expr_raw * expr_raw * expr_raw
   | Error_raw of string

(* expressions up to alpha equivalence *)
type expr = 
     Integer of int
   | Boolean of bool
   | Not of expr
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
   | Fn of expr
   | App of expr * expr
   | Let of expr * expr
   | Letrec of expr * expr
   | Cas of expr * expr * expr
   | Error of string

let rec string_of_expr e = match e with
    Integer n -> string_of_int n
  | Boolean true -> "true"
  | Boolean false -> "false"
  | Not e1 -> "¬(" ^ (string_of_expr e1) ^ ")"
  | Op (e1, op, e2) -> "(" ^ (string_of_expr e1) ^ " " ^
                       (string_of_op op) ^ " " ^ (string_of_expr e2) ^ ")"
  | If (e1, e2, e3) -> "if " ^ (string_of_expr e1) ^ " then " ^
                        (string_of_expr e2) ^ " else " ^ (string_of_expr e3)
  | Assign (e1, e2) -> "(" ^ (string_of_expr e1) ^ " := "
                           ^ (string_of_expr e2) ^ ")"
  | Deref e1 -> "!(" ^ (string_of_expr e1) ^ ")"
  | Ref e1 ->  "(ref " ^ string_of_expr e1 ^ ")"
  | Loc l ->  "Local:"^l
  | Glo l ->  "Global:"^l
  | Skip ->  "skip"
  | Seq (e1, e2) -> "(" ^ (string_of_expr e1) ^ " ; " ^ (string_of_expr e2) ^ ")"
  | While (e1, e2) ->  "while " ^ (string_of_expr e1) ^  " do " ^
                        (string_of_expr e2) ^  " done"
  | Var n ->  "V:" ^ string_of_int n
  | Fn (e1) -> "(fn O => " ^ (string_of_expr e1) ^ ")"
  | App (e1, e2) ->  "(" ^ (string_of_expr e1) ^ " " ^ (string_of_expr e2) ^ ")"
  | Let (e1, e2) ->  "let val O = " ^ (string_of_expr e1) ^
        " in " ^ (string_of_expr e2) ^  " end"
  | Letrec (e1, e2) -> "let rec O = (fn O => " ^ (string_of_expr e1)
                                 ^  ") in " ^ (string_of_expr e2) ^  " end"
  | Cas (e1, e2, e3) ->  "CAS(" ^ (string_of_expr e1) ^  ", " ^
                        (string_of_expr e2) ^  ", " ^ (string_of_expr e3) ^  ")"
  | Error msg ->  "(Error: " ^ msg ^ ")"

(*** SEMANTICS ***)

(* find_first : var_raw -> var_raw list -> int -> int option *)
(* find_first x ys 0 gives index of first occurance of x in ys *)
let rec find_first x zs m = match zs with
    [] -> None
  | y::ys -> if y=x then Some m else find_first x ys (m+1)

exception Resolve of string

(* resolve : var_raw list -> expr_raw -> expr *)
(* Given list of bound vars and raw expr. gives an de Bruijn expr.*)
let rec resolve env expr_raw = match expr_raw with
    Integer_raw n -> Integer n
  | Boolean_raw b -> Boolean b
  | Not_raw e -> Not (resolve env e)
  | Op_raw (e1, op, e2) -> Op (resolve env e1, op, resolve env e2)
  | If_raw (e1, e2, e3) -> If (resolve env e1,
                               resolve env e2,
                               resolve env e3)
  | Assign_raw (e1, e2) -> Assign (resolve env e1, resolve env e2)
  | Deref_raw e -> Deref (resolve env e)
  | Ref_raw e -> Ref (resolve env e)
  | Loc_raw l -> Loc l
  | Glo_raw l -> Glo l
  | Skip_raw -> Skip
  | Seq_raw (e1, e2) -> Seq (resolve env e1, resolve env e2)
  | While_raw (e1, e2) -> While(resolve env e1, resolve env e2)
  | Var_raw x -> (match find_first x env 0 with
                    None -> raise (Resolve ("Bogus unbound var_raw: " ^ x))
                  | Some n -> Var n)
  | Fn_raw (x, e) -> Fn (resolve (x::env) e)
  | App_raw (e1, e2) -> App (resolve env e1, resolve env e2)
  | Let_raw (x, e1, e2) -> Let (resolve env e1, resolve (x::env) e2)
  | Letrec_raw (x, y, e1, e2) ->
        Letrec (resolve (y::x::env) e1, resolve (x::env) e2)
  | Cas_raw (e1, e2, e3) -> Cas (resolve env e1,
                                 resolve env e2,
                                 resolve env e3)
  | Error_raw msg -> Error msg

let convert_from_raw re = resolve [] re

(* is_error : expr -> bool *)
(* is_error e if and only if e is stuck at an error *)
let rec is_error e = match e with
    Error _ -> true
  | Not e1 -> is_error e1
  | Op (e1, _, e2) -> is_error e1 || is_error e2
  | If (e1, _, _) -> is_error e1
  | Assign (e1, _) -> is_error e1
  | Deref e1 -> is_error e1
  | Ref e1 -> is_error e1
  | Seq (e1, _) -> is_error e1
  | While (e1, _) -> is_error e1
  | App (e1, _) -> is_error e1
  | Cas (e1, _, _) -> is_error e1
  | _ -> false

(* is_value : expr -> bool *)
let is_value e = match e with
    Integer _ -> true
  | Boolean _ -> true
  | Loc _ -> true
  | Glo _ -> true
  | Skip -> true
  | Fn (_) -> true
  | f -> is_error f

(* subst : expr -> int -> expr -> expr *)
(* subst e 0 f gives f with e substituted for the outmost variable *)
let rec subst e n f = match f with
    Integer m -> Integer m
  | Boolean b -> Boolean b
  | Not e1 -> Not (subst e n e1)
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
  | Fn (e1) -> Fn (subst e (n+1) e1)
  | App (e1, e2) -> App (subst e n e1, subst e n e2)
  | Let (e1, e2) -> Let (subst e n e1, subst e (n+1) e2)
  | Letrec (e1, e2) -> Letrec (subst e (n+2) e1, subst e (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (subst e n e1,
                             subst e n e2,
                             subst e n e3)
  | Error msg -> Error msg

(* To fulfill signature *)
let substitute_outmost e f = subst e 0 f

(* shift n e increments (by 1) all the variable indices >=n in e *)
let rec shift n e = match e with
    Integer m  -> Integer m
  | Boolean b -> Boolean b
  | Not e1 -> Not (shift n e1)
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
  | Fn (e1) -> Fn (shift (n+1) e1) 
  | App (e1, e2) -> App (shift n e1, shift n e2)
  | Let (e1, e2) -> Let (shift n e1, shift (n+1) e2)
  | Letrec (e1, e2) -> Letrec (shift (n+2) e1, shift (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (shift n e1, shift n e2, shift n e3)
  | Error msg -> Error msg


(* swap n e swaps the nth and n+1th variable indices in e *)
let rec swap n e = match e with
    Integer m -> Integer m
  | Boolean b -> Boolean b
  | Not e1 -> Not (swap n e1)
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
  | Fn (e1) -> Fn (swap (n+1) e1)
  | App (e1, e2) -> App (swap n e1, swap n e2)
  | Let (e1, e2) -> Let (swap n e1, swap (n+1) e2)
  | Letrec (e1, e2) -> Letrec (swap (n+2) e1, swap (n+1) e2)
  | Cas (e1, e2, e3) -> Cas (swap n e1, swap n e2, swap n e3)
  | Error msg -> Error msg

