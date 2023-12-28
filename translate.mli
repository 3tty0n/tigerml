type exp
type level
type access (* not the same as Frame.access *)

type newLevelParams = { parent : level; name : Temp.label; formals : bool list }

type frag = Frame.Frame.frag

val default_exp : exp

val outermost : level
val newLevel : newLevelParams -> level
val formals : level -> access list
val allocLocal : level -> bool -> access

val getResult : unit -> frag list
val init : unit -> unit
val unNx : exp -> Tree.stm

val simpleVar : access * level -> exp

val simpleVar : access * level -> exp
val subscriptVar : exp * exp -> exp
val fieldVar : exp * int -> exp
val assignExp : exp * exp -> exp
val arithmeticOperation : exp * Absyn.oper * exp -> exp
val comparisonOperation : exp * Absyn.oper * exp -> exp
val ifThenElse : exp * exp * exp -> exp
val ifThen : exp * exp -> exp
val stringExp : string -> exp
val recordExp : exp list -> exp
val arrayExp : exp * exp -> exp
val callExp : Temp.label * exp list * level * level -> exp
val whileExp : exp * exp * Temp.label -> exp
val forExp : exp * exp * exp * exp * Temp.label -> exp
val breakExp : Temp.label -> exp
val seqExp : exp list -> exp
val intExp : int -> exp
val procEntryExit : exp * level -> unit

(* ppx_deriving.show *)
val show_exp : exp -> string
