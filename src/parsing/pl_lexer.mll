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
  | "int" { TINT }
  | "unit" { TUNIT }
  | "bool" { TBOOL }
  | "rf" { TREF }
  | "->" { TARROW }
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
  | "G" id as lxm { GLO (lxm) }
  | "skip" { SKIP }
  | ";" { SEMICOLON }
  | "while" { WHILE }
  | "do" { DO }
  | "done" { DONE }
  | "V" id as lxm { VAR (lxm) }
  | "fn" { FN }
  | ":" { COLON }
  | "=>" { ARROW }
  | "@" { APP }
  | "let val" { LETVAL }
  | "let rec" { LETREC }
  | "in" { IN }
  | "cas" { CAS }
  | "," { COMMA }
  | "error(" (plain_text as msg) ")" { ERROR (msg) }
  | "eof" { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }


