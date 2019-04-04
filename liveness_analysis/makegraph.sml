structure MakeGraph :
sig
    structure Graph: FUNCGRAPH
    type nodeinfo = {def: Temp.temp list, use: Temp.temp list, ismove: bool}
    val instrs2graph: Assem.instr list -> nodeinfo Graph.graph * nodeinfo Graph.node list
end =
struct
    structure Graph = FuncGraph (
        struct
            type ord_key = int
            val compare = Int.compare
        end
    )
    type nodeinfo = {def: Temp.temp list, use: Temp.temp list, ismove: bool}

    fun instrs2graph instrList = (Graph.empty, [])
end
