
(**[interp_expr s] is a string representation of the result of
 * evaluating [s] as a zilang expression*)
val interp_expr: string -> string

(**[interp_expr s env = (t,env')] t is a string representation of the result
 * of evaluating [s] in the environment [env] where [env'] is the new
 * environment after the code was run*)
val interp_phrase: string -> Eval.env -> string * Eval.env
