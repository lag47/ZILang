(**The abstract syntax tree for zilang*)

(**A value id*)
type id = string

    (**The type of binary operators*)
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

      (**The type of unary operators*)
type unop =
  | Neg
  | Not

      (**The type of an abstract syntax tree for an expression in zilang*)
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

            (**The type of an abstract syntax tree in zilang*)
type phrase =
  | Expr of expr
  | Defn of id * expr
