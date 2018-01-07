open Ast

let make_int x =
  Int x

let make_var x =
  Var x

let make_bool b =
  Bool b

let make_equiv e1 e2 e3 =
  Equiv(e1,e2,e3)

let make_equiv_class e1 e2 =
  EquivClass(e1,e2)

let make_binop e1 b e2 =
  Binop(e1,b,e2)

let make_unop u e =
  Unop(u,e)

let make_fun s e =
  Fun(s,e)

let make_let s e1 e2 =
  Let(s,e1,e2)

let make_app e1 e2 =
  App(e1,e2)

let make_list es =
  List es

let make_if e1 e2 e3 =
  If(e1,e2,e3)
