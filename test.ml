open OUnit2
open Eval
open Prettyprint
open Parse
open Ast
open Main

let parse_tests =
  [
    (*basics*)
    "1", Int 1;
    "-1", Int (-1);
    "1 mod 5", EquivClass(Int 1,Int 5);
    "1 xeq 4 mod 5", Equiv(Int 1, Int 4, Int 5);
    "true", Bool true;
    "false", Bool false;
    "x", Var "x";

    "1 + 1", Binop (Int 1, Plus, Int 1);
    "1 - 1", Binop (Int 1, Minus, Int 1);
    "1 % 1", Binop (Int 1, Mod, Int 1);
    "1 = 1", Binop(Int 1, Equal, Int 1);
    "1 < 1", Binop(Int 1, LessThan, Int 1);
    "1 > 1", Binop(Int 1, GreaterThan, Int 1);
    "true and false", Binop (Bool true, And, Bool false);
    "-(1)", Unop(Neg, Int 1);
    "fun x -> x", Fun("x", Var "x");
    "fun x -> (x mod 5)", Fun ("x", EquivClass(Var "x", Int 5));
    "f x", App (Var "f", Var "x");
    "[]", List [];
    "[1]", List [Int 1];
    "[1;2]", List [Int 1; Int 2];
    "let x = 5 in x", Let("x", Int 5, Var "x");
    "if true then 1 else 2", If(Bool true, Int 1, Int 2);
    "(fun x -> x) 1", App (Fun("x", Var "x"), Int 1);
    "1 - (-1)", Binop (Int 1, Minus, Int(-1));
    "1 - (- (-1))", Binop ( Int 1, Minus, Unop(Neg,(Int (-1))));


  ]

let interp_tests =
  [
    "1", "1";
    "true", "true";
    "false", "false";
    "1 mod 5", "1 mod 5";
    "let x = 1 in x", "1";
    "if true then 1 else 2", "1";
    "if false then 1 else 2", "2";
    "let f = fun x -> fun y -> x + y in f 5 5", "10";

  ]
let make_parse_test i (a,b)  =
  "parse test" ^ string_of_int i  ^ ": " ^ a >::
  (fun _ -> assert_equal b (parse_expr a))

let make_interp_test i (a,b) =
  "interp test" ^ string_of_int i ^ ": " ^ a >::
  (fun _ -> assert_equal b (interp_expr a))

let _ = run_test_tt_main ("suite " >:::
  (List.mapi (fun idx (a,b) -> make_parse_test idx (a,b)) parse_tests)
  @ (List.mapi (fun idx (a,b) -> make_interp_test idx (a,b)) interp_tests))
