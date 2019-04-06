signature FRAME = sig
    type register = string
    val zero: Temp.temp option
    val RV: Temp.temp
    val FP: Temp.temp
    val argregs: Temp.temp list
    val calldefs: Temp.temp list
    val tempMap: register Temp.Map.map
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
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list
    val procEntryExit3: frame * Assem.instr list ->
        {prolog: string, body: Assem.instr list, epilog: string}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end
