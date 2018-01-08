open Ast
let zero =
  let z = Fun("f", (Fun ("x", Var "x")))
  in Defn("zero",z)

let one =
  let o = Fun("f", (Fun("x", App(Var "f", Var "x"))))
  in Defn ("one",o)

let decode =
  let d = Let("inc", (Fun( "x", Binop(Var "x", Plus, Int 1))),
              Fun("n", App(App(Var "n", Var "inc"), Int 0)))
  in Defn ("decode",d)


let plus =
  let p = Fun("m", (Fun ("n", (Fun ("f", (Fun ("x", App( App (Var "m", Var "f"),
                                                         (App(App(Var "n", Var "f"), Var "x"))))))))))
  in Defn ("plus",p)

let mult =
  let m = Fun("m", (Fun( "n", App( App (Var "m", App(Var "plus", Var "n")), Var "zero"))))
  in Defn ("mult",m)

let pow =
  let p = Fun("b", Fun("e", App (Var "e", Var "b")))
  in Defn ("pow",p)
