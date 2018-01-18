(**Construct expressions from component expressions*)

open Ast

val make_int : int -> expr

val make_var : id -> expr

val make_bool : bool -> expr

val make_equiv : expr -> expr -> expr -> expr

val make_equiv_class: expr -> expr -> expr

val make_binop : expr -> binop -> expr -> expr

val make_unop : unop -> expr -> expr

val make_fun : id -> expr -> expr

val make_let : id -> expr -> expr -> expr

val make_app : expr -> expr -> expr

val make_list : expr list -> expr

val make_if: expr -> expr -> expr -> expr
