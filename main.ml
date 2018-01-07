open Eval
open Parse
let interp s =
  s
  |> parse_expr
  |> fun e -> eval_expr e init_env
              |> string_of_result

let interp_phrase s env =
  s
  |> parse_phrase
  |> fun s -> eval_phrase s env
  |> fun (r,env1) -> (string_of_result r), env1
