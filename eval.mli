open Ast

type env

type value

type ('a, 'b) result

val init_env : env

val string_of_value : value -> string

val string_of_result: (value,string) result -> string

val eval_expr : expr -> env -> (value,string) result

val eval_phrase : phrase -> env -> (value,string) result * env
