type unique = unit ref [@@deriving show]

type ty =
  | INT
  | STRING
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * ty option ref
[@@deriving show]
