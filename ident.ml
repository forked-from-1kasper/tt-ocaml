type ident =
  | Irrefutable
  | Ident of string * int64

let showIdent : ident -> string = function
  | Ident (p, n) -> if !Prefs.indices then p ^ "#" ^ Int64.to_string n else p
  | Irrefutable  -> "_"

module IDENT = struct
  type t = ident
  let compare x y =
    match (x, y) with
    | Irrefutable, Irrefutable -> 0
    | Irrefutable, Ident _ -> -1
    | Ident _, Irrefutable -> 1
    | Ident (p, a), Ident (q, b) ->
      if p = q then compare a b
      else compare p q
end

module Env = Map.Make(IDENT)
module Files = Set.Make(String)

let getDigit x = Char.chr (Z.to_int x + 0x80) |> Printf.sprintf "\xE2\x82%c"

let ten = Z.of_int 10

let rec showSubscript x =
  if Z.lt x Z.zero then failwith "showSubscript: expected positive integer"
  else if Z.equal x Z.zero then "" else let (y, d) = Z.div_rem x ten in
    showSubscript y ^ getDigit d

let gidx : int64 ref = ref 0L
let gen () = gidx := Int64.succ !gidx; !gidx

let freshName x = let n = gen () in
  Ident (x ^ showSubscript (Z.of_int64 n), n)

let fresh : ident -> ident = function
  | Ident (p, _) -> Ident (p, gen ())
  | Irrefutable  -> Irrefutable
