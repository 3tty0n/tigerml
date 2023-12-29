type temp [@@deriving show]
val newtemp: unit -> temp
module Table: Table.S with type key=temp
val makestring: temp -> string

type label = Symbol.symbol [@@deriving show]
val newlabel: unit -> label
val namedlabel: string -> label
val string_of_label : label -> string
