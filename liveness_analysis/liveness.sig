signature LIVENESS =
sig
    structure Graph: FUNCGRAPH
    datatype igraph = IGRAPH of {
        graph: Temp.temp Graph.graph,
        tnode: Temp.temp -> Temp.temp Graph.node,
        moves: (Temp.temp Graph.node * Temp.temp Graph.node) list
    }

    val interferenceGraph:
        MakeGraph.nodeinfo MakeGraph.Graph.graph * MakeGraph.nodeinfo MakeGraph.Graph.node list
        -> igraph * (MakeGraph.nodeinfo MakeGraph.Graph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end