type id = string
type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Equal
  | LessThan
  | GreaterThan
  | And
  | Or
type unop =
  | Neg
  | Not

type expr =
  | Int of int
  | Bool of bool
  | Var of id
  | Equiv of expr * expr * expr
  | EquivClass of expr * expr
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Fun of id * expr
  | App of expr * expr
  | Let of id * expr * expr
  | List of expr list
  | If of expr * expr * expr

type phrase =
  | Expr of expr
  | Defn of id * expr
