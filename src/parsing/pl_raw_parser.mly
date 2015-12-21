%token TINT TUNIT TBOOL TREF TARROW
%token LBKT RBKT
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS MULT DIV MOD GT EQUALS
%token IF THEN ELSE
%token ASSIGN DEREF REF
%token <string> LOC GLO
%token SKIP
%token SEMICOLON
%token WHILE DO DONE
%token <string> VAR
%token FN COLON ARROW
%token APP
%token LETVAL LETREC IN
%token CAS COMMA
%token <string> ERROR
%token EOF

%right SEMICOLON (* anything ; anything groups the anythings together *)
%nonassoc ELSE IN (* ... else a @ b is else (a @ b), likewise for in *)
%left APP (* fn x => 5 @ 4 is (fn x => 5) @ 4 *)
%right ARROW (* fn x : t => 5 > 7 is => (5 > 7) *)

(* Standard arithmetic precedence *)
%nonassoc GT EQUALS
%left PLUS MINUS
%left MULT DIV MOD

%right ASSIGN (* ref a := !d is (ref a) := (!d) *)
%nonassoc DEREF REF (* !e and ref e bind most tightly *)

%right TARROW (* a -> b -> c is a -> (b -> c) *)
%nonassoc TREF (* a -> b rf is a -> (b rf) *)

%type <Type.type_expr> type_expr
%type <Pl_expression.expr_raw> expr parse_expr
%start parse_expr

%%

parse_expr:
  | e = expr EOF { e }

expr:
  | LBKT e = expr RBKT { e }
  | INT { Pl_expression.Integer_raw ($1) }
  | TRUE { Pl_expression.Boolean_raw true }
  | FALSE { Pl_expression.Boolean_raw false }
  | a = expr PLUS b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Plus, b) }
  | a = expr MINUS b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Minus, b) }
  | a = expr MULT b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Mult, b) }
  | a = expr DIV b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Div, b) }
  | a = expr MOD b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Mod, b) }
  | a = expr GT b = expr
     { Pl_expression.Op_raw (a, Pl_expression.GT, b) }
  | a = expr EQUALS b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Equals, b) }
  | IF b = expr THEN x = expr ELSE y = expr { Pl_expression.If_raw (b, x, y) }
  | a = expr ASSIGN b = expr { Pl_expression.Assign_raw (a, b) }
  | DEREF expr { Pl_expression.Deref_raw ($2) }
  | REF expr { Pl_expression.Ref_raw ($2) }
  | LOC { Pl_expression.Loc_raw ($1) }
  | GLO { Pl_expression.Glo_raw ($1) }
  | SKIP { Pl_expression.Skip_raw }
  | a = expr SEMICOLON b = expr { Pl_expression.Seq_raw (a, b) }
  | WHILE expr DO expr DONE { Pl_expression.While_raw ($2, $4) }
  | VAR { Pl_expression.Var_raw ($1) }
  | FN v = VAR COLON t = type_expr ARROW e = expr
        { Pl_expression.Fn_raw (v, t, e) }
  | a = expr APP b = expr { Pl_expression.App_raw (a, b) }
  | LETVAL v = VAR COLON t = type_expr
        EQUALS e = expr IN f = expr { Pl_expression.Let_raw (v, t, e, f) }
  | LETREC vx = VAR COLON t1 = type_expr EQUALS
        FN vy = VAR COLON t2 = type_expr ARROW e = expr IN f = expr
            { Pl_expression.Letrec_raw (vx, t1, vy, t2, e, f) }(*TODO Check types correct*)
  | CAS LBKT e1 = expr COMMA e2 = expr COMMA e3 = expr RBKT
        { Pl_expression.Cas_raw (e1, e2, e3) }
  | ERROR { Pl_expression.Error_raw ($1) }


type_expr:
  | LBKT t = type_expr RBKT { t }
  | TINT { Type.Int }
  | TUNIT { Type.Unit }
  | TBOOL { Type.Bool }
  | TREF t = type_expr { Type.Ref t }
  | t1 = type_expr TARROW t2 = type_expr { Type.Func (t1, t2) }
