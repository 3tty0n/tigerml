type pos = int [@@deriving show]

type var =
  | SimpleVar of Symbol.symbol * pos
  | FieldVar of var * Symbol.symbol * pos
  | SubscriptVar of var * exp * pos
[@@deriving show]

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of { func : Symbol.symbol; args : exp list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of {
      fields : (Symbol.symbol * exp * pos) list;
      typ : Symbol.symbol;
      pos : pos;
    }
  | SeqExp of (exp * pos) list
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of {
      var : Symbol.symbol;
      escape : bool ref;
      lo : exp;
      hi : exp;
      body : exp;
      pos : pos;
    }
  | BreakExp of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : Symbol.symbol; size : exp; init : exp; pos : pos }
[@@deriving show]

and dec =
  | FunctionDec of fundec list
  | VarDec of {
      name : Symbol.symbol;
      escape : bool ref;
      typ : (Symbol.symbol * pos) option;
      init : exp;
      pos : pos;
    }
  | TypeDec of typedec list
[@@deriving show]

and ty =
  | NameTy of Symbol.symbol * pos
  | RecordTy of field list
  | ArrayTy of Symbol.symbol * pos
[@@deriving show]

and oper =
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

and field = {
  name : Symbol.symbol;
  escape : bool ref;
  typ : Symbol.symbol;
  pos : pos;
}
[@@deriving show]

and fundec = {
  name : Symbol.symbol;
  params : field list;
  result : (Symbol.symbol * pos) option;
  body : exp;
  pos : pos;
}
[@@deriving show]

and typedec = { name : Symbol.symbol; ty : ty; pos : pos } [@@deriving show]
