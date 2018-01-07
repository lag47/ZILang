open OUnit2
open Eval
open Prettyprint
open Parse
open Ast

let parse_tests =
  [
    "1", Int 1;
    "true", Bool true;
    "false", Bool false;
    "x", Var "x";
    "1 + 1", Binop (Int 1, Plus, Int 1);
    "1 - 1", Binop (Int 1, Minus, Int 1);
    "1 - (-1)", Binop (Int 1, Minus, Int(-1));
    "1 - (- (-1))", Binop ( Int 1, Minus, Unop(Neg,(Int (-1))));
    "[]", List [];
    "[1]", List [Int 1];
    "[1;2]", List [Int 1; Int 2]

  ]

let make_parse_test i (a,b)  =
  "test" ^ string_of_int i  ^ ": " ^ a >::
  (fun _ -> assert_equal b (parse_expr a))

let _ = run_test_tt_main ("suite " >:::
    List.mapi (fun idx (a,b) -> make_parse_test idx (a,b)) parse_tests)
