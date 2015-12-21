let print_result str = print_endline (Pl_expression.string_of_expr (
    Pl_expression.convert_from_raw (
        Pl_parser.parse_expr Pl_lexer.read (Lexing.from_string (str^" eof")))))

(* X for end of parsing *)
let _ = List.map print_result [

    "37"; "true"; "false"; "5 + 6"; "5 * 3"; "9 % 100";
    "if true then 34 else 67"; "45 := true"; "!Gooo";
    "ref 4"; "skip"; "skip"; "if false then true else true";
    "while false do 4 / 0 done"; "fn Vx : int => Vx + 5"; "4 @ 9";
    "let val Vx : bool = true in if Vx then 4 else 5";
    "let rec Vf : bool -> unit = fn Vx : bool => skip in 4";
    "cas(Goo, 4, 0)"; "if true then error(rubbish fish) else skip"
]
