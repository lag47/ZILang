open Ast

let pad s =
  " " ^ s ^ " "

let print_binop =
  function
  | Plus -> pad "+"
  | Minus -> pad "-"
  | Mult -> pad "*"
  | Div -> pad "/"
  | Mod -> pad "mod"
  | Equal -> pad "="
  | LessThan -> "<"
  | GreaterThan -> ">"
  | And -> "and"
  | Or -> "or"

let print_unop =
  function
  | Neg -> pad "-"
  | Not -> pad "not"

let rec pretty_print e =
  match e with
  | Int(x) -> x |> string_of_int |> pad
  | Bool(b) -> if b then pad "true" else pad "false"
  | Var(s) -> pad s
  | Equiv(e1,e2,e3) ->
    (pretty_print e1) ^ (pad "xeq") ^
    (pretty_print e2) ^ (pad "mod") ^ (pretty_print e3)
  | EquivClass(e1,e2) ->
    (pretty_print e1) ^ (pad "mod") ^ (pretty_print e2)
  | Binop(e1,b,e2) ->
    (pretty_print e1) ^ (print_binop b) ^ (pretty_print e2)
  | Unop(u,e) ->
    (print_unop u) ^ (pretty_print e)
  | Fun(s,e) ->
    "fun" ^ (pad s) ^ pad "->" ^ pretty_print e
  | App(e1,e2) ->
    (pretty_print e1) ^ " " ^ (pretty_print e2)
  | Let(s,e1,e2) ->
    "let " ^ s ^ " = " ^ (pretty_print e1) ^ " in " ^ (pretty_print e2)
  | List(es) ->
    let f = fun acc e -> acc ^ (pretty_print e) ^ ", "
    in "[" ^ List.fold_left f "" es ^ "]"
  | If(e1,e2,e3) -> pad "if" ^ (pretty_print e1) ^ pad "then" ^
                    (pretty_print e2) ^ pad "else" ^ (pretty_print e3)
