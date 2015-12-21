%token PLUS MULT EOF
%token <int> N
%type <int> parse_product product sum
%start parse_product

%%

parse_product:
 | product EOF { $1 }
product:
 | a = product MULT b = product { a * b }
 | a = sum { a }

sum:
 | a = sum PLUS b = sum {a + b}
 | N { $1 }
