let print_result str = print_endline (string_of_int (Toy_parser.parse_product Toy_lexer.read (Lexing.from_string str)))

(* X for end of parsing *)
let _ = List.map print_result ["1X"; "1+1X"; "2 + 1X"; "2 * 2X"; "1 * 2 + 3X"; "3 *2X"; "2+2 + 2 + 1 X "; "2 * 2 + 2X"]

