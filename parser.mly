%{
open Ast
open Astfactory

%}

%token <int> INT
%token <string> ID
%token <bool> BOOL

%token PLUS MINUS MULT DIV MODOP MOD ARROW FUN EQ XEQ LET IN IF THEN ELSE SEMICOLON NOT
%token LESS GREATER AND OR
%token RPAREN LPAREN RBRACK LBRACK
%token EOF
(*%nonassoc SEMICOLON*)
(*%nonassoc THEN*)
%nonassoc ELSE
%right OR
%right AND
%left PLUS MINUS MODOP
%left MULT DIV MOD
%right ARROW EQ XEQ


%start <Ast.expr> parse_expression
%start <Ast.phrase> parse_phrase

%%

parse_expression:
  | e = expr; EOF
    { e }

parse_phrase:
  | e = expr; SEMICOLON?; EOF
      { Expr e }
  | d = defn; SEMICOLON; EOF
      { d }


defn:
  | LET; x = ID; EQ; e= expr
    { Defn (x,e) }

expr:
  | e = simpl_expr
    { e }
  | e1 = expr; e2 = expr
    { make_app e1 e2}
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { make_if e1 e2 e3 }
  | LET; x = ID; EQ; e1 = expr; IN; e2 = expr
    { make_let x e1 e2}
  | uop = unop; e = expr
    { make_unop uop e}
  | e1 = expr; b = binop; e2 = expr
    { make_binop e1 b e2}
  | FUN; x = ID; ARROW; e = expr
    { make_fun x e}
  | e1 = expr; MOD; e2 = expr
    { make_equiv_class e1 e2}
  | e1 = expr; XEQ; e2 = expr; MOD; e3 = expr
    { make_equiv e1 e2 e3}


simpl_expr:
  | x = ID
    {make_var x}
  | x = INT
    {make_int x}
  | b = BOOL
    { make_bool b }
  | LBRACK; es = separated_list(SEMICOLON,expr); RBRACK
    {make_list es}
  | LPAREN; e = expr; RPAREN
    { e }

unop:
  | MINUS
    { Neg }
  | NOT
    { Not }

binop:
  | PLUS
    {Plus}
  | MINUS
    { Minus }
  | MULT
    {Mult}
  | DIV
    {Div}
  | MODOP
    {Mod}
  | EQ
    {Equal}
  | LESS
    {LessThan}
  | GREATER
    { GreaterThan }
  | AND
    {And}
  | OR
    {Or}
