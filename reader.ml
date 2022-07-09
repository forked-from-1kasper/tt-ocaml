open Lexing

let parseErr f lexbuf =
  try f Lexer.main lexbuf
  with Parser.Error ->
    raise (Error.Parser (lexbuf.lex_curr_p.pos_lnum, lexeme lexbuf))

let parse filename =
  let chan = open_in filename in
  Printf.printf "Parsing “%s”.\n" filename;
  Error.handleErrors
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      let file = parseErr Parser.file lexbuf in
      print_endline (Module.showFile file))
    chan ()