signature TEMP = 
sig
    eqtype temp
    val newtemp: unit -> temp
    (* Each temp must have a unique ID *)
    val tempID: temp -> int
    structure Table: TABLE sharing type Table.key = temp
    val makestring: temp -> string

    type label = Symbol.symbol
    val newlabel: unit -> label
    val namedlabel: string -> label
end

