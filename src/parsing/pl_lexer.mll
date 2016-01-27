{
  open Pl_raw_parser
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
  | "(" { LBKT }
  | ")" { RBKT }
  | nat as lxm { INT (int_of_string lxm) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "%" { MOD }
  | ">" { GT }
  | "=" { EQUALS }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ":=" { ASSIGN }
  | "!" { DEREF }
  | "ref" { REF }
  | "L" (id as lxm) { LOC (lxm) }
  | "G" (id as lxm) { GLO (lxm) }
  | "skip" { SKIP }
  | ";" { SEMICOLON }
  | "while" { WHILE }
  | "do" { DO }
  | "done" { DONE }
  | "fn" { FN }
  | "=>" { ARROW }
  | "@" { APP }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "cas" { CAS }
  | "," { COMMA }
  | "error(" (plain_text as msg) ")" { ERROR (msg) }
  | ":" { COLON }
  | "###" "#"* { HASHBREAK }
  | "eof" { EOF }
  | (id as lxm) { VAR (lxm) }
  | _ { raise (SyntaxError ("Unexpected char \"" ^ Lexing.lexeme lexbuf ^
        "\" at position " ^ (string_of_int (Lexing.lexeme_start lexbuf)))) }

