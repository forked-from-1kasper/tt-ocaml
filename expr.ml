open Ident

type exp =
  | EKan of Z.t                                                               (* cosmos *)
  | EVar of ident | EHole                                                  (* variables *)
  | EPi of exp * (ident * exp) | ELam of exp * (ident * exp) | EApp of exp * exp   (* Π *)
  | ESig of exp * (ident * exp) | EPair of exp * exp | EFst of exp | ESnd of exp   (* Σ *)

type tele = ident * exp

type value =
  | VKan of Z.t
  | Var of ident * value | VHole
  | VPi of value * clos | VLam of value * clos | VApp of value * value
  | VSig of value * clos | VPair of value * value | VFst of value | VSnd of value

and clos = ident * (value -> value)

type term = Exp of exp | Value of value

type ctx =
  { local  : (value * value) Env.t;
    global : (value * term) Env.t ref }

let eLam p a b = ELam (a, (p, b))
let ePi  p a b = EPi  (a, (p, b))
let eSig p a b = ESig (a, (p, b))

let ident x = Ident (x, 0L)
let decl x  = EVar (ident x)

let upVar p x ctx = match p with Irrefutable -> ctx | _ -> Env.add p x ctx

let upLocal ctx p t v = { ctx with local = upVar p (t, v) ctx.local }
let upGlobal ctx p t v = ctx.global := upVar p (t, v) !(ctx.global)

let rec telescope ctor e : tele list -> exp = function
  | (p, a) :: xs -> ctor p a (telescope ctor e xs)
  | []           -> e

let parens b x = if b then "(" ^ x ^ ")" else x

let rec ppExp paren e = let x = match e with
  | EKan n -> "U" ^ showSubscript n
  | ELam (a, (p, b)) -> Printf.sprintf "λ %s, %s" (showTele p a) (showExp b)
  | EPi (a, (p, b)) -> showPiExp a p b
  | ESig (a, (p, b)) -> Printf.sprintf "Σ %s, %s" (showTele p a) (showExp b)
  | EPair (fst, snd) -> Printf.sprintf "(%s, %s)" (showExp fst) (showExp snd)
  | EFst exp -> ppExp true exp ^ ".1"
  | ESnd exp -> ppExp true exp ^ ".2"
  | EApp (f, x) -> Printf.sprintf "%s %s" (showExp f) (ppExp true x)
  | EVar p -> showIdent p | EHole -> "?"
  in match e with
  | EKan _ | EVar _ | EHole
  | EFst _ | ESnd _ | EPair _ -> x
  | _ -> parens paren x

and showExp e = ppExp false e
and showTele p x = Printf.sprintf "(%s : %s)" (showIdent p) (showExp x)

and showPiExp a p b = match p with
  | Irrefutable -> Printf.sprintf "%s → %s" (ppExp true a) (showExp b)
  | _           -> Printf.sprintf "Π %s, %s" (showTele p a) (showExp b)

(* Readback *)
let rec rbV = function
  | VLam (t, g)  -> rbVTele eLam t g
  | VPair (u, v) -> EPair (rbV u, rbV v)
  | VKan u       -> EKan u
  | VPi (t, g)   -> rbVTele ePi t g
  | VSig (t, g)  -> rbVTele eSig t g
  | Var (x, _)   -> EVar x
  | VApp (f, x)  -> EApp (rbV f, rbV x)
  | VFst k       -> EFst (rbV k)
  | VSnd k       -> ESnd (rbV k)
  | VHole        -> EHole
and rbVTele ctor t (p, g) = let x = Var (p, t) in ctor p (rbV t) (rbV (g x))

let showValue v = showExp (rbV v)

let showTerm : term -> string = function Exp e -> showExp e | Value v -> showValue v

let showGamma (ctx : ctx) : string =
  Env.bindings ctx.local
  |> List.map (fun (p, (t, _)) -> Printf.sprintf "%s : %s" (showIdent p) (showValue t))
  |> String.concat "\n"
