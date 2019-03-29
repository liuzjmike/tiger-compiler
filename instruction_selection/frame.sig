signature FRAME = sig
    type register = string
    val FP: Temp.temp
    val RV: Temp.temp
    val tempMap: register Temp.Table.table
    val wordSize: int

    val externalCall: string * Tree.exp list -> Tree.exp

    type access
    type frame
    val newFrame: {name: Temp.label,
                   formals: bool list} -> frame
    val name: frame -> string
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    val string: Tree.label * string -> string

    val exp: access -> Tree.exp -> Tree.exp

    val procEntryExit1: frame * Tree.stm -> Tree.stm

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end