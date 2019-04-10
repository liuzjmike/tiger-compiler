signature LIVENESS =
sig
    structure Graph: FUNCGRAPH
    datatype igraph = IGRAPH of {
        graph: unit Graph.graph,
        tnode: Temp.temp -> unit Graph.node,
        moves: (unit Graph.node * unit Graph.node) list
    }

    val interferenceGraph:
        MakeGraph.nodeinfo MakeGraph.Graph.graph -> MakeGraph.nodeinfo MakeGraph.Graph.node
        -> igraph * (MakeGraph.nodeinfo MakeGraph.Graph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end