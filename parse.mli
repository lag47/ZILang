exception SyntaxError of string

(**[parse_expr s] is the abstract syntax tree of the expression represented
 * by [s]*)
val parse_expr: string -> Ast.expr

(**[parse_phrase s] is the abstract syntax tree of the phrase represented
 * by [s]*)
val parse_phrase: string -> Ast.phrase
