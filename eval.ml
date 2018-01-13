open Ast
open Church_numeral
module Env = Map.Make(String)
open Env
open Equiv
type extern =
  | Solve

type value =
  | VInt of int
  | VEquiv of int * int * int
  | VEquivClass of int * int
  | VList of value list
  | Extern of extern
  | Closure of id * expr * env
  | VBool of bool

and env = value Env.t

type 'a result =
  | Value of 'a
  | Exception of string

let return v =
  Value v

let bind r f =
  match r with
  | Value v -> f v
  | Exception _ -> r

let map r f =
  match r with
  | Value v -> return (f v)
  | Exception _ -> r

let join r =
  match r with
  | Value (Value v) -> Value v
  | Value (Exception s) -> Exception s
  | Exception s -> Exception s

let (>>=) = bind

let (>>|) = map



let rec string_of_value v =
  match v with
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VEquiv(i1,i2,i3) ->
    string_of_int i1 ^ " xeq "
    ^ string_of_int i2 ^ " mod " ^ string_of_int i3
  | VEquivClass(i1,i2) ->
    string_of_int i1 ^ " mod " ^ string_of_int i2
  | VList vs ->
    let m = List.fold_left (fun acc v -> acc ^ string_of_value v ^ ";") "" vs
    in "[" ^ m ^ "]"
  | Closure (id,e, _) -> "<closure>"
  | Extern e -> "<closure>"

let string_of_result r =
  match r with
  | Exception s -> "Exception:" ^ s
  | Value v -> string_of_value v



let normalize_class i1 i2 =
  if i2 <= 0
  then Exception "equivence classes must be modulo a positive integer"
  else if i1 >= 0
  then let rep = i1 mod i2 in Value (VEquivClass(rep,i2))
  else let rep = (i1 - (i1 * i2)) mod i2 in Value (VEquivClass(rep,i2))

let rec compute_binop v1 b v2 =
    match b with
    | Plus -> compute_plus v1 v2
    | Minus -> compute_minus v1 v2
    | Mult -> compute_mult v1 v2
    | Div -> compute_div v1 v2
    | Mod -> compute_mod v1 v2
    | And -> compute_and v1 v2
    | Or -> compute_or v1 v2
    | Equal -> compute_equal v1 v2
    | LessThan -> compute_less v1 v2
    | GreaterThan -> compute_greater v1 v2


and list_binop l1 b l2 =
  if List.length l1 <> List.length l2
  then Exception "lists are not the same length"
  else let rs = List.map2 (fun l r -> compute_binop l b r) l1 l2 in
    if List.exists (fun r-> match r with Exception _ -> true | _ -> false) rs
    then List.find (fun r-> match r with Exception _ -> true | _ -> false) rs
    else
      Value
        (VList
           (List.map (fun r -> match r with Value v -> v
                                          | _ -> failwith "guarded against") rs))

and compute_less v1 v2 =
  match v1,v2 with
  | VInt(i1),VInt(i2) -> Value (VBool (i1<i2))
  | VList(l1), VList(l2) -> list_binop l1 LessThan l2
  | _ -> Exception "type error"

and compute_greater v1 v2 =
  match v1,v2 with
  | VInt(i1),VInt(i2) -> Value (VBool (i1>i2))
  | VList(l1), VList(l2) -> list_binop l1 GreaterThan l2
  | _ -> Exception "type error"

and compute_equal v1 v2 =
  Value (VBool (v1 = v2))

and compute_plus v1 v2 =
  match v1,v2 with
  | VInt(i1),VInt(i2) -> Value(VInt(i1 + i2))
  | VEquivClass(a1,m1),VEquivClass(a2,m2) ->
    if m1 = m2
    then  normalize_class (a1+a2) m1
    else Exception "cannot add equivalence classes with different bases"
  | VList(l1), VList(l2) -> list_binop l1 Plus l2
  | _ -> Exception "type error with plus"

and compute_minus v1 v2 =
  match v1,v2 with
  | VInt(i1),VInt(i2) -> Value (VInt (i1 - i2))
  | VEquivClass(a1,m1), VEquivClass(a2,m2) ->
    if m1 = m2
    then normalize_class (a1-a2) m1
    else Exception "cannot add equivalence classes with different bases"
  | VList(l1), VList(l2) -> list_binop l1 Minus l2
  | _ -> Exception "type error with minus"

and compute_div v1 v2 =
  match v1,v2 with
  | VInt(i1), VInt(i2) ->
    if i2 = 0 then Exception "division by zero"
    else Value (VInt(i1/i2))
  | _ -> Exception "type error with divide"

and compute_mult v1 v2 =
  match v1,v2 with
  | VInt(i1), VInt(i2) -> Value (VInt(i1 * i2))
  | _ -> Exception "type error with mult"

and compute_mod v1 v2 =
  match v1,v2 with
  | VInt(i1), VInt(i2) ->
    if i2 = 0 then Exception "division by zero"
    else Value (VInt(i1 mod i2))
  | _ -> Exception "type error with mod"

and compute_and v1 v2 =
  match v1,v2 with
  | VBool(b1), VBool(b2) -> Value(VBool (b1 && b2))
  | _ -> Exception "type error with and"

and compute_or v1 v2 =
  match v1,v2 with
  | VBool(b1), VBool(b2) -> Value(VBool (b1 || b2))
  | _ -> Exception "type error with or"





let compute_neg v =
  match v with
  | VInt(i) -> Value (VInt (-i))
  | VEquivClass(a,m) -> Value (VEquivClass((-a),m))
  | _ -> Exception "type error with neg"

let compute_not v =
  match v with
  | VBool(b) -> Value (VBool(not b ))
  | _ -> Exception "type error with not"


let rec compute_unop u v =
  match u with
  | Neg -> compute_neg v
  | Not -> compute_not v

let compute_equiv a b m =
  match Equiv.solvemod a m b with
  | Solution (k,n) -> normalize_class k n
  | Nosol s -> Exception s

let compute_chin vs =
  let is_class = function
    | VEquivClass _ -> true
    | _ -> false
  in
  let format = function
    | VEquivClass (a,m) ->(a,m)
    | _ -> failwith "failed precon"
  in
  if List.for_all is_class vs
  then let r = chin_rem (List.map format vs) in
    match r with
    | Solution (a,m) -> normalize_class a m
    | Nosol s -> Exception s
  else Exception "Must be a list of equivalence classes"


let compute_extern e v =
  match e,v with
  | Solve, VEquiv(a,b,m) -> compute_equiv a b m

  | Solve, VList vs -> compute_chin vs
  | _ -> failwith "should be unreachable"

let reverse vlst =
  match vlst with
  | VList(vs) -> Value (VList (List.rev vs))
  | _ -> Exception "can only reverse lists"

let expect_bool v =
  match v with
  | Value (VBool(b)) ->  v
  | _ -> Exception "expected a bool"

let to_bool v =
  match v with
  | VBool(b) -> b
  | _ -> failwith "failed bool precond"

let expect_int v =
  match v with
  | Value (VInt(i)) -> v
  | _ -> Exception "expected an int"

let to_int v =
  match v with
  | VInt(i) -> i
  | _ -> failwith "failed bool precond"

let rec eval_expr expr env =
  match expr with
  | Int(x) -> Value (VInt(x))
  | Bool(b) -> Value (VBool b)
  | Var(x) -> eval_var x env
  | Equiv(e1,e2,e3) ->
    eval_equiv e1 e2 e3 env
  | EquivClass(e1,e2) ->
    eval_equiv_class e1 e2 env
  | Binop(e1,b, e2) ->
    eval_binop e1 e2 b env
  | Unop(u,e) ->
    eval_unop u e env
  | Fun(s,e) ->
    eval_fun s e env
  | App(e1,e2) ->
    eval_app e1 e2 env
  | Let(s,e1,e2) ->
    eval_let s e1 e2 env
  | List(es) -> eval_list es env
  | If(e1,e2,e3) -> eval_if e1 e2 e3 env


and eval_var s env =
  let v_opt = find_opt s env in
  match v_opt with
  | Some v -> Value v
  | None -> Exception "unbound variable"

and eval_equiv e1 e2 e3 env =
  eval_expr e1 env |> expect_int >>=
  fun v1 -> eval_expr e2 env |> expect_int >>=
  fun v2 -> eval_expr e3 env |> expect_int >>=
  fun v3 ->
  Value(VEquiv(to_int v1, to_int v2, to_int v3))

and eval_equiv_class e1 e2 env =
  eval_expr e1 env |> expect_int >>=
  fun v1 -> eval_expr e2 env |> expect_int >>=
  fun v2 ->
  normalize_class (to_int v1) (to_int v2)


and eval_binop e1 e2 b env =
  eval_expr e1 env >>=
  fun v1 -> eval_expr e2 env >>=
  fun v2 ->
  compute_binop v1 b v2

and eval_unop u e env =
  eval_expr e env >>=
  fun v -> compute_unop u v

and eval_fun s e env =
  Value (Closure(s,e,env))

and eval_app e1 e2 env =
  eval_expr e1 env  >>=
  fun v1 ->
  eval_expr e2 env >>=
  fun v2 ->
  match v1 with
  | Closure (s, e, envc) ->
    let envc1 = add s v2 envc in eval_expr e envc1
  | Extern x -> compute_extern x v2
  | _ -> Exception "only functions can be applied"

and eval_let s e1 e2 env =
  eval_expr e1 env >>=
  fun v1 ->
  let env1 = add s v1 env in eval_expr e2 env1

and eval_list es env =
  let f r e=
    r >>=
    fun vlast ->
    match vlast with
    | VList acc ->
      eval_expr e env >>=
      fun v -> Value (VList (v::acc))
    | _ -> failwith "should never occur"
  in
  List.fold_left f (Value (VList [])) es >>=
  fun v -> reverse v

and eval_if e1 e2 e3 env =
  eval_expr e1 env |>
  expect_bool >>=
  fun b ->
  if to_bool b
    then eval_expr e2 env
    else eval_expr e3 env

let eval_defn id e env =
  let r = eval_expr e env in
  match r with
  | Value v -> r, (add id v env)
  | Exception _ -> r, env

let eval_phrase p env =
  match p with
  | Expr e -> (eval_expr e env),env
  | Defn(id,e) -> eval_defn id e env


let init_env =
  let defs = [zero; one; decode; plus; mult; pow;]
  in List.fold_left
    (fun env def -> let (_,new_env) = eval_phrase def env in new_env)
    empty defs
     |> Env.add "solve" (Extern (Solve))
