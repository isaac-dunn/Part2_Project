{
  open Pl_parser
  exception SyntaxError of string
}

let nat = ['0' - '9']+
let white = [' ' '\t' '\n' '\r']+
(* Digits allowed at beginning as will always be prefixed by L, G or V *)
let id = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let plain_text = ['a'-'z' 'A'-'Z' '_' ' ' '0'-'9']+

rule read =
  parse
  | white { read lexbuf }
  | "int" { TINT }
  | "unit" { TUNIT }
  | "bool" { TBOOL }
  | "rf" { TREF }
  | "->" { TARROW }
  | "(" { LBRACKET }
  | ")" { RBRACKET }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }


