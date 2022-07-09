open Module
open Ident
open Error
open Decl
open Elab
open Expr

let help =
"Available commands:
  <statement>    infer type and normalize statement
  :q             quit
  :r             restart
  :h             display this message"

let main : command -> unit = function
  | Eval e -> let (t, v) = checkAndEval (freshExp e) in
    Printf.printf "TYPE: %s\nEVAL: %s\n" (showValue t) (showValue v)
  | Action "q" -> exit 0
  | Action "r" -> ctx.global := Env.empty; raise Restart
  | Action "h" -> print_endline help
  | Command (s, _) | Action s -> raise (UnknownCommand s)
  | Nope -> ()

let fs = ref Files.empty
let check filename = fs := handleErrors (checkFile !fs) filename !fs

let banner = "TT theorem prover, version 0.0.1"

let repl () =
  print_endline ("\n" ^ banner) ;
  try while true do
    print_string "> ";
    let line = read_line () in
    handleErrors (fun x -> let cmd = Reader.parseErr Parser.repl (Lexing.from_string x) in
      main cmd) line ()
  done with End_of_file -> ()