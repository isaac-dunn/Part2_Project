open Type

let print_result str = print_endline (string_of_type_expr (
    Pl_parser.type_expr Pl_lexer.read (Lexing.from_string str)))

(* X for end of parsing *)
let _ = List.map print_result ["int"; "bool"; "unit"; "rf int";
            "rf rf bool"; "(bool -> unit)"; "rf (bool -> (int -> unit))";
            "((bool->bool)->(bool->bool))"]

