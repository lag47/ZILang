open Eval
open Parse
let interp_expr s =
  try
  s
  |> parse_expr
  |> fun e -> eval_expr e init_env
  |> string_of_result
  with Parse.SyntaxError s | Failure s -> s

let interp_phrase s env =
  try
  s
  |> parse_phrase
  |> fun s -> eval_phrase s env
  |> fun (r,env1) -> (string_of_result r), env1
  with
    Parse.SyntaxError s | Failure s -> s,env
  | End_of_file -> "", env
