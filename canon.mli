val linearize : Tree.stm -> Tree.stm list

val basicBlocks : Tree.stm list -> Tree.stm list list * Temp.label

val traceSchedule : Tree.stm lsit list * Temp.label -> Tree.stm list
