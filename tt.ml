open Error

type cmdline =
  | Check of string
  | Parse of string
  | Repl   | Help
  | Trace  | Indices
  | Girard | Silent

let help =
"\n  invocation = tt | tt list
        list = [] | command list

     command = check <filename> | help
             | parse <filename> | girard
             | trace            | indices
             | silent           | repl"

let repl = ref false
let cmd : cmdline -> unit = function
  | Check filename -> Repl.check filename
  | Parse filename -> Reader.parse filename
  | Help -> print_endline Repl.banner; print_endline help
  | Repl -> repl := true
  | Silent -> Prefs.verbose := false
  | Indices -> Prefs.indices := true
  | Trace -> Prefs.indices := true; Prefs.trace := true
  | Girard -> Prefs.girard := true

let rec parseArgs : string list -> cmdline list = function
  | [] -> []
  | "check"     :: filename :: rest -> Check filename :: parseArgs rest
  | "parse"     :: filename :: rest -> Parse filename :: parseArgs rest
  | "help"      :: rest             -> Help    :: parseArgs rest
  | "trace"     :: rest             -> Trace   :: parseArgs rest
  | "indices"   :: rest             -> Indices :: parseArgs rest
  | "silent"    :: rest             -> Silent  :: parseArgs rest
  | "girard"    :: rest             -> Girard  :: parseArgs rest
  | "repl"      :: rest             -> Repl    :: parseArgs rest
  | x :: xs -> Printf.printf "Unknown command “%s”\n" x; parseArgs xs

let defaults = function
  | [] -> [Help]
  | xs -> xs

let rec main () =
  try Array.to_list Sys.argv |> List.tl |> parseArgs |> defaults |> List.iter cmd;
    if !repl then Repl.repl () else ()
  with Restart -> main ()

let () = main ()
