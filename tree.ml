type exp =
  | CONST of int
  | NAME of Temp.label
  | TEMP of Temp.temp
  | BINOP of binop * exp * exp
  (* Treat exp as memory address, can be used as left-value and right-value *)
  | MEM of exp
  (* Apply f to list of l.  *)
  | CALL of exp * exp list
  (* The statement is evaluated for side effects, then the expression
     is evaluated for a result *)
  | ESEQ of stm * exp
[@@deriving show]

and stm =
  (* MOVE (TEMP t, e) Evaluate e and move the result to temporary *)
  (* MOVE (MEM e1, e2). Evaluate e and results in address a. Then evaluated e2 and
     store its result into wordSize bytes of memory starting at a. *)
  | MOVE of exp * exp
  (* Evaluates the expression and discards the result *)
  | EXP of exp
  (* JUMP(e, labs) Jump to address e. The destination e may be a literal label, e.g.
     NAME(lab), or an address calculated by other expressions. The list of labels
     specifies all the possible locations that the expression e can evaluate to; this
     is necessary for dataflow analysis later.
     To jump to a known label: JUMP(NAME l, [l]) *)
  | JUMP of exp * Temp.label list
  (* Evaluate e1, e2 in that order, yielding values a,b. Then compare a, b using the
     relational operator o. If the result is true, jump to t; otherwise jump to f. *)
  | CJUMP of relop * exp * exp * Temp.label * Temp.label
  (* The statement s1 follows s2 *)
  | SEQ of stm * stm
  (* ABEL(n) Define the constant value of name n to be the current machine code
     address. This is like a label definition in assembly language. After which
     NAME(n) may be the target of jumps, calls, etc  *)
  | LABEL of Temp.label
[@@deriving show]

and binop = PLUS | MINUS | MUL | DIV
          | AND | OR | LSHIFT | RSHIFT | XOR (* | ARSHIFT *)
[@@deriving show]

and relop = EQ | NE | LT | GT | LE | GE (* | ULT | ULE | UGT | UGE *)
[@@deriving show]

let not_rel = function
  | EQ -> NE
  | NE -> EQ
  | LT -> GE
  | GT -> LE
  | LE -> GT
  | GE -> LT
