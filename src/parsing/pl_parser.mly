%token TINT TUNIT TBOOL TREF TARROW
%token LBKT RBKT
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS MULT DIV MOD GT EQUALS
%token IF THEN ELSE
%token ASSIGN DEREF  REF
%token <string> GLO
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

%type <Type.type_expr> type_expr
%type <Pl_expression.expr_raw> expr parse_expr
%start parse_expr

%%

parse_expr:
  | e = expr EOF { e }

expr:
  | INT { Pl_expression.Integer_raw ($1) }
  | TRUE { Pl_expression.Boolean_raw true }
  | FALSE { Pl_expression.Boolean_raw false }
  | LBKT expr PLUS expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Plus, $4) }
  | LBKT expr MINUS expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Minus, $4) }
  | LBKT expr MULT expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Mult, $4) }
  | LBKT expr DIV expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Div, $4) }
  | LBKT expr MOD expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Mod, $4) }
  | LBKT expr GT expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.GT, $4) }
  | LBKT expr EQUALS expr RBKT
     { Pl_expression.Op_raw ($2, Pl_expression.Equals, $4) }
  | IF b = expr THEN x = expr ELSE y = expr { Pl_expression.If_raw (b, x, y) }
  | LBKT expr ASSIGN expr RBKT { Pl_expression.Assign_raw ($2, $4) }
  | DEREF expr { Pl_expression.Deref_raw ($2) }
  | REF expr { Pl_expression.Ref_raw ($2) }
  | GLO { Pl_expression.Glo_raw ($1) }
  | SKIP { Pl_expression.Skip_raw }
  | LBKT expr SEMICOLON expr RBKT { Pl_expression.Seq_raw ($2, $4) }
  | WHILE expr DO expr DONE { Pl_expression.While_raw ($2, $4) }
  | VAR { Pl_expression.Var_raw ($1) }
  | FN v = VAR COLON t = type_expr ARROW e = expr
        { Pl_expression.Fn_raw (v, t, e) }
  | LBKT expr APP expr RBKT { Pl_expression.App_raw ($2, $4) }
  | LETVAL v = VAR COLON t = type_expr
        EQUALS e = expr IN f = expr { Pl_expression.Let_raw (v, t, e, f) }
  | LETREC vx = VAR COLON t1 = type_expr EQUALS
        FN vy = VAR COLON t2 = type_expr ARROW e = expr IN f = expr
            { Pl_expression.Letrec_raw (vx, t1, vy, t2, e, f) }(*TODO Check types correct*)
  | CAS LBKT e1 = expr COMMA e2 = expr COMMA e3 = expr RBKT
        { Pl_expression.Cas_raw (e1, e2, e3) }
  | ERROR { Pl_expression.Error_raw ($1) }


type_expr:
  | TINT { Type.Int }
  | TUNIT { Type.Unit }
  | TBOOL { Type.Bool }
  | TREF t = type_expr { Type.Ref t }
  | LBKT t1 = type_expr
      TARROW t2 = type_expr RBKT { Type.Func (t1, t2) }
