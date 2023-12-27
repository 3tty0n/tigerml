module Sym = Symbol
module Pos = Position

type oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
[@@deriving show]

type exp =
  | NilExp
  | IntExp of int
  | StringExp of { string : string; pos : Pos.t }
  | CallExp of { func : Sym.t; args : exp list; pos : Pos.t }
  | OpExp of { left : exp; oper : oper; right : exp; pos : Pos.t }
  | RecordExp of { fields : (Sym.t * exp * Pos.t) list; typ : Sym.t; pos : Pos.t }
  | SeqExp of (exp * Pos.t) list
  | AssignExp of { var : var; exp : exp; pos : Pos.t }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : Pos.t }
  | WhileExp of { test : exp; body : exp;  pos : Pos.t }
  | ForExp of { var : Sym.t; escape : bool ref; lo : exp; hi : exp; body : exp; pos : Pos.t }
  | BreakExp of Pos.t
  | LetExp of { decs : dec list; body : exp; pos : Pos.t  }
  | ArrayExp of { typ : Sym.t; size : exp; init : exp; pos : Pos.t }
  | VarExp of var
[@@deriving show]

and var =
  | SimpleVar of { symbol : Sym.t; pos : Pos.t }
  | FieldVar of { var : var; symbol : Sym.t ; pos : Pos.t }
  | SubScriptVar of { var : var; exp : exp; pos : Pos.t }
[@@deriving show]

and dec =
  | VarDec of { name : Sym.t; escape : bool ref; typ : (Sym.t * Pos.t) option; init : exp; pos : Pos.t }
  | TypeDec of typedec list
  | FunctionDec of fundec list
[@@deriving show]

and ty =
  | NameTy of { symbol : Sym.t; pos : Pos.t }
  | RecordTy of field list
  | ArrayTy of { symbol : Sym.t; pos : Pos.t }
[@@deriving show]

and field =
    Field of { name : Sym.t; escape : bool ref; typ : Sym.t; pos : Pos.t }
[@@deriving show]

and typedec = { name : Sym.t; ty : ty; pos : Pos.t }
[@@deriving show]

and fundec = { name : Sym.t; params : field list; result : (Sym.t * Pos.t) option; body : exp; pos : Pos.t }
[@@deriving show]

type t = exp
[@@deriving show]
