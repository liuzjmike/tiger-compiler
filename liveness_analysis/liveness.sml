structure Liveness : LIVENESS =
struct
    structure Graph = FuncGraph (
        struct
            type ord_key = int
            val compare = Int.compare
        end
    )
    datatype igraph = IGRAPH of {
        graph: Temp.temp Graph.graph,
        tnode: Temp.temp -> Temp.temp Graph.node,
        moves: (Temp.temp Graph.node * Temp.temp Graph.node) list
    }

    fun interferenceGraph flowGraph = 
        let val graph = Graph.empty
            val (graph, node) = Graph.addNode' (graph, 0, Temp.newtemp ())
        in (
            IGRAPH {
                graph=graph,
                tnode=fn temp => node,
                moves=[]
            },
            fn flowNode => []
        )
        end

    fun show (outstream, IGRAPH {graph, tnode, moves}) = ()
end