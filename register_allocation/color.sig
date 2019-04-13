signature COLOR =
sig
    structure Frame : FRAME
    structure Graph : FUNCGRAPH
    type allocation = Frame.register Temp.Map.map
    val color : {
        interference: Liveness.igraph,
        initial: allocation,
        spillCost: 'a Graph.node -> int,
        registers: Frame.register list
    } -> allocation * Temp.temp list
end