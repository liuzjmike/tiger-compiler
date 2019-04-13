signature LIVENESS =
sig
    structure Graph: FUNCGRAPH where type nodeID = Temp.temp
    datatype igraph = IGRAPH of {
        graph: unit Graph.graph,
        moves: unit Graph.graph
    }

    val interferenceGraph:
        MakeGraph.nodeinfo MakeGraph.Graph.graph -> MakeGraph.nodeinfo MakeGraph.Graph.node
            -> igraph * (MakeGraph.nodeinfo MakeGraph.Graph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end