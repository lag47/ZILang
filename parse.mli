exception SyntaxError of string

val parse_expr: string -> Ast.expr

val parse_phrase: string -> Ast.phrase
