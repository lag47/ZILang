open Ast

(**[env] is an environment that maps identifiers to values*)
type env

(**[value] is the type of zilang values*)
type value

(**['a result] represents the result of some computation that
 * can either by a value of type 'a or an exception of type string*)
type 'a result

(**[init_env] is the initial environment, with only basic values bound*)
val init_env : env

(**[string_of_value v] is a string representation of the value v*)
val string_of_value : value -> string

(**[string_of_result r] is a string representation of the value result r,
 * if are is [Value v] then [string_of_value v] otherwise the string
 * carried with the exception*)
val string_of_result: value result -> string

(**[eval_expr e env] evaluates the expression e in the context of the
 * environment env to a value result*)
val eval_expr : expr -> env -> value result

(**[eval_phrase p env] evaluates the phrase p in the context of environment
 * env to the value result representing the result of the computation
 * and the new environment*)
val eval_phrase : phrase -> env -> value result * env
