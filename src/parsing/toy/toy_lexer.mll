{
type token = MULT | PLUS | N of int | EOF
exception SyntaxError of string
}

let int = ['0' - '9'] ['0' - '9']*
let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }
  | "*" { MULT }
  | "+" { PLUS }
  | int { N (int_of_string (Lexing.lexeme lexbuf)) }
  | "X" { EOF }

