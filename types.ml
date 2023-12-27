module S = Symbol

type unique = unit ref
[@@deriving show]

type ty =
  | RECORD of (unit -> (S.t * S.t) list) * unique
  | NIL
  | INT
  | STRING
  | ARRAY of ty * unique
  | NAME of S.t * ty option ref
  | UNIT
  | BOTTOM
[@@deriving show]

type comp =
  | LT
  | GT
  | EQ
  | INCOMP
[@@deriving show]

let leq t1 t2 =
  match t1, t2 with
  | BOTTOM, _ -> true
  | NIL, RECORD (_) -> true
  | INT, INT -> true
  | STRING, STRING -> true
  | RECORD (_, unique1), RECORD (_, unique2) -> unique1 = unique2
  | ARRAY (_, unique1), ARRAY (_, unique2) -> unique1 = unique2
  | NIL, NIL -> true
  | NAME (sym1, _), NAME (sym2, _) -> String.compare (S.name sym1) (S.name sym2) = 0
  | _, _ -> false

let comp t1 t2 =
  if leq t1 t2 && leq t1 t2 then EQ
  else if leq t1 t2 then LT
  else if leq t1 t2 then GT
  else INCOMP

let is_int = function
  | INT -> true
  | _ -> false

let is_string = function
  | STRING -> true
  | _ -> false
