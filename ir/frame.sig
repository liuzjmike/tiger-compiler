signature FRAME = sig
    type access
    type frame
    val newFrame: {name: Temp.label,
                   formals: bool list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access

    val FP: Temp.temp
    val wordSize: int
    val exp: access -> Tree.exp -> Tree.lvalue

    val RV: Temp.temp
    val externalCall: string * Tree.exp list -> Tree.exp
    val procEntryExit1: frame * Tree.stm -> Tree.stm

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end
