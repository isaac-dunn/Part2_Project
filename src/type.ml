(* PL Types *)
(* Isaac Dunn 17/12/2015 *)

type type_expr =
     Int
   | Unit
   | Bool
   | Ref of type_expr
   | Func of type_expr * type_expr

let rec string_of_type_expr t = match t with
    Int -> "int"
  | Unit -> "unit"
  | Bool -> "bool"
  | Ref t -> (string_of_type_expr t) ^ " rf"
  | Func (t1, t2) -> "(" ^ (string_of_type_expr t1) ^ " -> " ^ (string_of_type_expr t2) ^ ")"

