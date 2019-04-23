signature LIVENESS =
sig
    structure Graph: FUNCGRAPH where type nodeID = Temp.temp
    datatype igraph = IGRAPH of {
        graph: unit Graph.graph,
        moves: unit Graph.graph
    }

    (* Given a flow graph and a list of nodes, runs liveness analysis
    and builds interference graph on the nodes in the order given.
    Returns an interference graph and a mapping from a node in the
    flow graph to the temps live-out at the node *)
    val interferenceGraph:
        MakeGraph.graph -> MakeGraph.node list
            -> igraph * (MakeGraph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end