%token TINT TUNIT TBOOL TREF TARROW
%token LBRACKET RBRACKET
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS MULT DIV MOD GT EQUALS
%token IF THEN ELSE
%token ASSIGN DEREF  REF
%token <string> LOC GLO
%token SKIP
%token SEMICOLON
%token WHILE DO DONE
%token <string> VAR
%token FN COLON ARROW
%token APP
%token LETVAL LETREC IN END
%token CAS COMMA
%token <string> ERROR

%type <Type.type_expr> type_expr
%start type_expr

%%

type_expr:
  | TINT { Type.Int }
  | TUNIT { Type.Unit }
  | TBOOL { Type.Bool }
  | TREF t = type_expr { Type.Ref t }
  | LBRACKET t1 = type_expr
      TARROW t2 = type_expr RBRACKET { Type.Func (t1, t2) }
