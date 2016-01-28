%token LBKT RBKT
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS MULT DIV MOD LT GT EQUALS
%token NOT AND OR
%token IF THEN ELSE
%token ASSIGN DEREF REF
%token <string> LOC GLO SPINLOCK
%token SKIP
%token SEMICOLON
%token WHILE DO DONE
%token <string> VAR
%token FN ARROW
%token APP
%token LET REC IN
%token CAS COMMA
%token LOCK UNLOCK
%token <string> ERROR
%token HASHBREAK COLON
%token EOF

%nonassoc ELSE IN (* ... else a @ b is else (a @ b), likewise for in *)
%right SEMICOLON
%left APP (* fn x => 5 @ 4 is (fn x => 5) @ 4 *)
%right ARROW (* fn x : t => 5 > 7 is => (5 > 7) *)

%right ASSIGN (* a := b + c is a := (b + c) *)

(* Standard operator precedence *)
%nonassoc GT LT EQUALS AND OR
%left PLUS MINUS
%left MULT DIV MOD

%nonassoc NOT DEREF REF LOCK UNLOCK (* These bind most tightly *)

%type <Pl_expression.expr_raw> expr parse_expr
%type <(string * Pl_expression.expr_raw) list * (Pl_expression.expr_raw list)> parse_program
%start parse_expr parse_program

%%

parse_program:
  | g_store thread_list { ($1, $2) }

g_store:
  | HASHBREAK { [] }
  | v = VAR COLON e = expr gs = g_store { (v, e)::gs }

thread_list:
  | e = expr EOF { [e] }
  | e = expr HASHBREAK tl = thread_list { e::tl }

parse_expr:
  | e = expr EOF { e }

expr:
  | LBKT e = expr RBKT { e }
  | INT { Pl_expression.Integer_raw ($1) }
  | MINUS INT { Pl_expression.Integer_raw (-$2) }
  | TRUE { Pl_expression.Boolean_raw true }
  | FALSE { Pl_expression.Boolean_raw false }
  | NOT e = expr { Pl_expression.Not_raw e }
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
  | a = expr LT b = expr
     { Pl_expression.Op_raw (a, Pl_expression.LT, b) }
  | a = expr EQUALS b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Equals, b) }
  | a = expr AND b = expr
     { Pl_expression.Op_raw (a, Pl_expression.And, b) }
  | a = expr OR b = expr
     { Pl_expression.Op_raw (a, Pl_expression.Or, b) }
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
  | FN v = VAR ARROW e = expr
        { Pl_expression.Fn_raw (v, e) }
  | a = expr APP b = expr { Pl_expression.App_raw (a, b) }
  | LET v = VAR EQUALS e = expr
        IN f = expr { Pl_expression.Let_raw (v, e, f) }
  | LET REC vx = VAR EQUALS
        FN vy = VAR ARROW e = expr IN f = expr
            { Pl_expression.Letrec_raw (vx, vy, e, f) }
  | SPINLOCK { Pl_expression.Spinlock_raw ($1) }
  | LOCK e = expr { Pl_expression.Lock_raw e }
  | UNLOCK e = expr { Pl_expression.Unlock_raw e }
  | CAS LBKT e1 = expr COMMA e2 = expr COMMA e3 = expr RBKT
        { Pl_expression.Cas_raw (e1, e2, e3) }
  | ERROR { Pl_expression.Error_raw ($1) }

