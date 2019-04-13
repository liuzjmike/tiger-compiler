signature REG_ALLOC =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Map.map
    val alloc : Assem.instr list * Frame.frame
        -> Assem.instr list * allocation
end