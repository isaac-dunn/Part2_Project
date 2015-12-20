(* Types *)
(* Isaac Dunn 17/12/2015 *)

type type_expr =
     Int
   | Unit
   | Bool
   | Ref of type_expr
   | Func of type_expr * type_expr

val string_of_type_expr : type_expr -> string
