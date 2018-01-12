open Ast

(**[env] is an environment that maps identifiers to values*)
type env

(**[value] is the type of zilang values*)
type value

(**['a result] represents the result of some computation that
 * can either by a value of type 'a or an exception of type string*)
type 'a result

(**[init_env] is the initial environment, with only basic *)
val init_env : env

val string_of_value : value -> string

val string_of_result: value result -> string

val eval_expr : expr -> env -> value result

val eval_phrase : phrase -> env -> value result * env
