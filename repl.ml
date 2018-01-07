(* Acknowledgement:
 * This REPL is adapated from the sample REPL provided as part of the
 * Lambda-Term package (c) 2015 by Martin DeMello, released under BSD3.
 * as well as teh JoCaml REPL released with A4 of CS3110 in fall 2017
 * by Michael Clarkson*)

open React
open Lwt
open LTerm_text

exception Quit

module Interpreter = struct
  type repl_state = {
    command_count : int;
    env : Eval.env;
  }

  let initial_rstate = {
    command_count = 1;
    env = Eval.init_env;
  }

  let quit_regex  = Str.regexp {|^#quit\(;;\)?$|}

  let matches s r =
    Str.string_match r s 0

  let eval state s =
    if matches s quit_regex then
      raise Quit
    else
      let (out, env') = Main.interp_phrase s state.env in
      let state' = {
        command_count = state.command_count + 1;
        env = env';
      } in
      (state', out)
end

let make_prompt state =
  let prompt = "# " in
  eval [ S prompt ]

let make_output state out =
  let output =
    if out = "" then ""
    else Printf.sprintf "%s\n" out in
  eval [ S output ]

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.const (make_prompt state))
end

let rec loop term history state =
  Lwt.catch (fun () ->
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
      rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    let state, out = Interpreter.eval state command in
    LTerm.fprints term (make_output state out)
    >>= fun () ->
    LTerm_history.add history command;
    loop term history state
  | None ->
    loop term history state

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch (fun () ->
      let state = Interpreter.initial_rstate in
      Lazy.force LTerm.stdout
      >>= fun term ->
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt | Quit -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
