(* Acknowledgement:  the lexing code for strings, integers, identifiers
 *  and comments is adapted from the OCaml 4.04 lexer
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/lexer.mll],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1.
 * and from the lexer released as part of A4 in CS 3110 at Cornell University
 * [https://http://www.cs.cornell.edu/courses/cs3110/2017fa/] written by
 * Michael Clarkson*)
{
  open Lexing
  open Parser
  exception Error
  let comment_depth = ref 0
}


let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id =  ('_' | letter ) ('_' | letter | digit)*
let blank =  [' ' '\009' '\012']

rule token = parse
  | blank+
    { token lexbuf }
  | ['\n']
    { new_line lexbuf; token lexbuf}
  | int
    {INT (int_of_string (Lexing.lexeme lexbuf))}
  | "(*"
    { incr comment_depth;
      comment lexbuf;
      token lexbuf}
  | "true"
    {BOOL (true)}
  | "false"
    {BOOL (false)}
  | "+"
    {PLUS}
  | "-"
    {MINUS}
  | "*"
    { MULT }
  | "/"
    {DIV}
  | "%"
    {MODOP}
  | "->"
    {ARROW}
  | "fun"
    {FUN}
  | "xeq"
    { XEQ}
  | "let"
    { LET }
  | "in"
    { IN }
  | "if"
    { IF }
  | "then"
    { THEN }
  | "else"
    { ELSE}
  | "and"
    {AND}
  | "or"
    {OR}
  | ";"
    { SEMICOLON }
  | "mod"
    { MOD }
  | "not"
    { NOT }
  | ")"
    { RPAREN }
  | "("
    { LPAREN }
  | "]"
    { RBRACK }
  | "["
    { LBRACK }
  | id
    {ID (Lexing.lexeme lexbuf)}
  | eof
    { EOF }
  | _
    { raise Error }

and comment = parse
  | "(*"
    {incr comment_depth;
      comment lexbuf }
  | "*)"
  { decr comment_depth;
        let d = !comment_depth in
        if d = 0 then ()
        else if d > 0 then comment lexbuf
        else assert false
        }
