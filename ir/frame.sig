signature FRAME = sig
    type access
    type frame
    datatype frag = PROC of {body: Tree.stm, frame: frame} | STRING of Temp.label * string

    val RV : Temp.temp
    val FP : Temp.temp
    val wordSize: int
    val name : frame -> Temp.label
    (* val exp : access -> Tree.exp -> Tree.exp *)
    (* val newFrame : {name: Temp.label, formals: bool list} -> frame *)
    val formals : frame -> access list
    (* val allocLocal : frame -> bool -> access *)
    (* val externalCall: string * Tree.exp list -> Tree.exp *)
    (* val procEntryExitl : frame * Tree.stm -> Tree.stm *)
end
