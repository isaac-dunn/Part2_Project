let expr_raw_of_string s = Pl_raw_parser.parse_expr Pl_lexer.read (Lexing.from_string (s^" eof"))

let expr_of_string s = Pl_expression.convert_from_raw (expr_raw_of_string s)

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.Lexing.pos_fname
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let program_of_lexbuf lexbuf =
    let (g_raw, es_raw) =
        try Pl_raw_parser.parse_program Pl_lexer.read lexbuf with
        | Pl_lexer.SyntaxError msg ->
            Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg; exit(-1)
        | Pl_raw_parser.Error ->
            Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1)
    in let g = List.rev_map
        (fun (s, e) -> (s, Pl_expression.convert_from_raw e)) g_raw
    in let convert e_str = (Pl_expression.convert_from_raw e_str, Store.PLStore.empty)
    in let tds = Array.of_list (List.rev_map convert es_raw)
    in (tds, g)

let program_of_string s =
    program_of_lexbuf (Lexing.from_string (s^" eof"))

let program_of_channel ic =
    program_of_lexbuf (Lexing.from_channel ic)

let print_result str = print_endline (Pl_expression.string_of_expr (expr_of_string str))

(* X for end of parsing *)
let f () = List.map print_result [

    "37"; "true"; "false"; "5 + 6"; "5 * 3"; "9 % 100";
    "if true then 34 else 67"; "45 := true"; "!Gooo";
    "ref 4"; "skip"; "skip"; "if false then true else true";
    "while false do 4 / 0 done"; "fn Vx : int => Vx + 5"; "4 @ 9";
    "let val Vx : bool = true in if Vx then 4 else 5";
    "let rec Vf : bool -> unit = fn Vx : bool => skip in 4";
    "cas(Goo, 4, 0)"; "if true then error(rubbish fish) else skip";
    "1 + 2 + 3"; "2 + 2 * 2"; "2 * 2 + 2"; "2 * 3 > 2 + 3";
    "if true then skip else ref 4 := 5; skip";
    "let val Vt:int=6 in fn Vx : int => Vt + 3 * Vx @ 4 ; skip";
    "let val Vt:int=6 in (fn Vx : int => Vt + 3 * Vx @ 4 ; skip)";
    "ref 0 := 1; ref 0 := 2; ref 0 := 3; cas(Gx, 0, 4); ref 0 := 5";
]
