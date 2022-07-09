open Module
open Check
open Error
open Ident
open Expr
open Elab

let ext x = x ^ ".tt"

let ctx : ctx = { local = Env.empty; global = ref Env.empty }

let assign x t e =
  if Env.mem (ident x) !(ctx.global)
  then raise (AlreadyDeclared x)
  else upGlobal ctx (ident x) t e

let getDeclName : decl -> string = function Def (p, _, _) | Axiom (p, _) -> p
let getTerm e = if !Prefs.preeval then Value (eval ctx e) else Exp e

let checkAndEval e : value * value =
  (Check.infer ctx e, Check.eval ctx e)

let checkDecl = function
  | Def (x, Some t, e) ->
    ignore (extKan (infer ctx t)); let t' = eval ctx t in
    check ctx e t'; assign x t' (getTerm e)
  | Def (x, None, e) ->
    assign x (infer ctx e) (getTerm e)
  | Axiom (x, t) ->
    ignore (extKan (infer ctx t)); let t' = eval ctx t in
    assign x t' (Value (Var (ident x, t')))

let getBoolVal opt = function
  | "tt" | "true"  -> true
  | "ff" | "false" -> false
  | value -> raise (UnknownOptionValue (opt, value))

let rec checkLine fs = function
  | Decl d ->
    if !Prefs.verbose then begin
      Printf.printf "Checking: %s\n" (getDeclName d); flush_all ()
    end; checkDecl (freshDecl d); fs
  | Option (opt, value) ->
    begin match opt with
      | "girard"   -> Prefs.girard  := getBoolVal opt value
      | "verbose"  -> Prefs.verbose := getBoolVal opt value
      | "pre-eval" -> Prefs.preeval := getBoolVal opt value
      | _          -> raise (UnknownOption opt)
    end; fs
  | Import xs -> List.fold_left (fun fs x -> let path = ext x in
    if Files.mem path fs then fs else checkFile fs path) fs xs
and checkFile fs path =
  let filename = Filename.basename path in

  let chan = open_in path in
  let file = Reader.parseErr Parser.file (Lexing.from_channel chan) in
  close_in chan; if !Prefs.verbose then begin
    Printf.printf "Parsed “%s” successfully.\n" filename; flush_all ()
  end;

  let res = checkContent (Files.add path fs) file in
  print_endline ("File “" ^ filename ^ "” checked."); res
and checkContent fs xs = List.fold_left checkLine fs xs
