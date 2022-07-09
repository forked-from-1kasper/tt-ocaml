open Error
open Trace
open Ident
open Elab
open Expr

let ieq u v : bool = !Prefs.girard || u = v

let vfst : value -> value = function
  | VPair (u, _) -> u
  | v            -> VFst v

let vsnd : value -> value = function
  | VPair (_, u) -> u
  | v            -> VSnd v

let idfun t = VLam (t, (freshName "x", fun x -> x))

(* Evaluator *)
let rec eval (ctx : ctx) (e0 : exp) = traceEval e0; match e0 with
  | EKan u                -> VKan u
  | EVar x                -> getDef ctx x
  | EHole                 -> VHole
  | EPi  (a, (p, b))      -> let t = eval ctx a in VPi (t, (fresh p, closByVal ctx p t b))
  | ESig (a, (p, b))      -> let t = eval ctx a in VSig (t, (fresh p, closByVal ctx p t b))
  | ELam (a, (p, b))      -> let t = eval ctx a in VLam (t, (fresh p, closByVal ctx p t b))
  | EApp (f, x)           -> app (eval ctx f, eval ctx x)
  | EPair (e1, e2)        -> VPair (eval ctx e1, eval ctx e2)
  | EFst e                -> vfst (eval ctx e)
  | ESnd e                -> vsnd (eval ctx e)

and closByVal ctx p t e v = traceClos e p v;
  (* dirty hack to handle free variables introduced by type checker *)
  let ctx' = match v with
  | Var (x, t) -> if Env.mem x ctx.local then ctx else upLocal ctx x t v
  | _          -> ctx in
  eval (upLocal ctx' p t v) e

and app (f, x) = match f, x with
  (* (λ (x : t), f) v ~> f[x/v] *)
  | VLam (_, (_, f)), v -> f v
  | _, _ -> VApp (f, x)

and app2 f x y = app (app (f, x), y)

and getType ctx x = match Env.find_opt x ctx.local, Env.find_opt x !(ctx.global) with
  | Some (t, _), _ -> t
  | _, Some (t, _) -> t
  | _, _           -> raise (VariableNotFound x)

and getDef ctx x = match Env.find_opt x ctx.local, Env.find_opt x !(ctx.global) with
  | Some (_, v), _       -> v
  | _, Some (_, Value v) -> v
  | _, Some (t, Exp e)   -> let v = eval ctx e in upGlobal ctx x t (Value v); v
  | _, _                 -> raise (VariableNotFound x)

(* This is part of evaluator, not type checker *)
and inferV v = traceInferV v; match v with
  | Var (_, t) -> t
  | VLam (t, (x, f)) -> VPi (t, (x, fun x -> inferV (f x)))
  | VPi (t, (x, f)) | VSig (t, (x, f)) -> imax (inferV t) (inferV (f (Var (x, t))))
  | VFst e -> inferFst (inferV e)
  | VSnd e -> inferSnd (vfst e) (inferV e)
  | VApp (f, x) -> let (_, (_, g)) = extPi (inferV f) in g x
  | VKan n -> VKan (Z.succ n)
  | VPair _ | VHole -> raise (InferVError v)

and inferFst = function
  | VSig (t, _) -> t
  | v           -> raise (ExpectedSig v)

and inferSnd v = function
  | VSig (_, (_, g)) -> g v
  | u                -> raise (ExpectedSig u)

(* Convertibility *)
and conv v1 v2 : bool = traceConv v1 v2;
  v1 == v2 || begin match v1, v2 with
    | VKan u, VKan v -> ieq u v
    | VPair (a, b), VPair (c, d) -> conv a c && conv b d
    | VPair (a, b), v | v, VPair (a, b) -> conv (vfst v) a && conv (vsnd v) b
    | VPi  (a, (p, f)), VPi  (b, (_, g))
    | VSig (a, (p, f)), VSig (b, (_, g))
    | VLam (a, (p, f)), VLam (b, (_, g)) ->
      let x = Var (p, a) in conv a b && conv (f x) (g x)
    | VLam (a, (p, f)), b | b, VLam (a, (p, f)) ->
      let x = Var (p, a) in conv (app (b, x)) (f x)
    | VApp (f, x), VApp (g, y) -> conv f g && conv x y
    | Var (u, _), Var (v, _) -> u = v
    | VFst x, VFst y | VSnd x, VSnd y -> conv x y
    | _, _ -> false
  end

and eqNf v1 v2 : unit = traceEqNF v1 v2;
  if conv v1 v2 then () else raise (Ineq (v1, v2))

(* Type checker itself *)

and check ctx (e0 : exp) (t0 : value) =
  traceCheck e0 t0; try match e0, t0 with
  | EHole, v -> traceHole v ctx
  | ELam (a, (p, b)), VPi (t, (_, g)) ->
    ignore (extKan (infer ctx a)); eqNf (eval ctx a) t;
    let x = Var (p, t) in let ctx' = upLocal ctx p t x in check ctx' b (g x)
  | EPair (e1, e2), VSig (t, (_, g)) ->
    ignore (extKan (inferV t));
    check ctx e1 t; check ctx e2 (g (eval ctx e1))
  | e, t -> eqNf (infer ctx e) t
  with ex -> Printf.printf "When trying to typecheck\n  %s\nAgainst type\n  %s\n" (showExp e0) (showValue t0); raise ex

and infer ctx e : value = traceInfer e; try match e with
  | EVar x -> getType ctx x
  | EKan u -> VKan (Z.succ u)
  | ESig (a, (p, b)) | EPi (a, (p, b)) -> inferTele ctx p a b
  | ELam (a, (p, b)) -> inferLam ctx p a b
  | EApp (f, x) -> begin match infer ctx f with
    | VPi (t, (_, g)) -> check ctx x t; g (eval ctx x)
    | v -> raise (ExpectedPi v)
  end
  | EFst e -> inferFst (infer ctx e)
  | ESnd e -> inferSnd (vfst (eval ctx e)) (infer ctx e)
  | EPair _ | EHole -> raise (InferError e)
  with ex -> Printf.printf "When trying to infer type of\n  %s\n" (showExp e); raise ex

and inferInd fibrant ctx t e f =
  let (t', (p, g)) = extPi (infer ctx e) in eqNf t t'; let k = g (Var (p, t)) in
  ignore (if fibrant then extKan k else extKan k); f (eval ctx e)

and inferTele ctx p a b =
  ignore (extKan (infer ctx a));
  let t = eval ctx a in let x = Var (p, t) in
  let ctx' = upLocal ctx p t x in
  let v = infer ctx' b in imax (infer ctx a) v

and inferLam ctx p a e =
  ignore (extKan (infer ctx a)); let t = eval ctx a in
  ignore (infer (upLocal ctx p t (Var (p, t))) e);
  VPi (t, (p, fun x -> infer (upLocal ctx p t x) e))

and mem x = function
  | Var (y, _) -> x = y
  | VSig (t, (p, f)) | VPi (t, (p, f)) | VLam (t, (p, f)) ->
    mem x t || mem x (f (Var (p, t)))
  | VKan _ | VHole -> false
  | VFst e | VSnd e -> mem x e
  | VPair (a, b) | VApp (a, b) -> mem x a || mem x b
