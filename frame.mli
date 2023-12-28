type exp
type frame
type access
type newFrameParams = { name: Temp.label; formals : bool list }

val exp : unit
val newFrame : newFrameParams -> frame
val formals : frame -> access list
val allocLocal : frame -> bool -> access
