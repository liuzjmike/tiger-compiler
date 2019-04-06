structure Liveness : LIVENESS =
struct
    structure MG = MakeGraph
    structure Graph = FuncGraph (
        struct
            type ord_key = Temp.temp
            val compare = Temp.compare
        end
    )
    structure Table = IntMapTable (
        type key = MG.nodeinfo MG.Graph.node
		fun getInt node = MG.Graph.getNodeID node
    )

    datatype igraph = IGRAPH of {
        graph: Temp.temp Graph.graph,
        tnode: Temp.temp -> Temp.temp Graph.node,
        moves: (Temp.temp Graph.node * Temp.temp Graph.node) list
    }

    fun postOrderDFS (node, graph, visited, result) = (
        case Table.look (visited, node)
        of  SOME () => (result, visited)
        |   NONE =>
            let val visited = Table.enter (visited, node, ())
                val result = node::result
                fun foldSucc (succ, (result, visited)) =
                    postOrderDFS (succ, graph, visited, result)
            in foldl foldSucc (result, visited) (MG.Graph.succs' graph node)
            end
    )

    fun postOrderNodes graph =
        let fun foldNode (node, (list, visited)) =
                let val (result, visited) = postOrderDFS (node, graph, visited, [])
                in (list @ result, visited)
                end
            val (nodeList, visited) = foldl foldNode ([], Table.empty) (MG.Graph.nodes graph)
        in nodeList
        end

    fun iterToFixedPoint f init list =
        let val (result, changed) = foldl f (init, false) list
        in if changed then iterToFixedPoint f result list else result
        end

    fun interferenceGraph flowGraph = 
        let val postList = postOrderNodes flowGraph

            (* Liveness analysis *)
            fun updateLiveness (node, (liveMap, changed)) =
                let val (oldLI, oldLO) =
                        case Table.look (liveMap, node)
                        of  SOME v => v
                        |   NONE => (Temp.Set.empty, Temp.Set.empty)
                    fun foldSucc (succ, liveOut) =
                        case Table.look (liveMap, succ)
                        of  SOME (succLI, succLO) => Temp.Set.union (liveOut, succLI)
                        |   NONE => liveOut
                    val newLO = foldl foldSucc oldLO (MG.Graph.succs' flowGraph node)
                    fun tempSetDelete (item, set) =
                        Temp.Set.delete (set, item)
                        handle NotFound => set
                in
                    if Temp.Set.equal (oldLO, newLO)
                    then (liveMap, changed)
                    else
                        let val {def, use, ismove} = MG.Graph.nodeInfo node
                            val newLI = foldl tempSetDelete newLO def
                            val newLI = foldl Temp.Set.add' newLI use
                        in (Table.enter (liveMap, node, (newLI, newLO)), true)
                        end
                end
            val liveMap = iterToFixedPoint updateLiveness Table.empty postList

            (* Build interference graph *)
            fun addNode (graph, temp) =
                (graph, Graph.getNode (graph, temp))
                handle Graph.NoSuchNode id => Graph.addNode' (graph, id, temp)
            fun addEdge (graph, temp1, temp2) =
                let val (graph, node1) = addNode (graph, temp1)
                    val (graph, node2) = addNode (graph, temp2)
                in Graph.doubleEdge (graph, Graph.getNodeID node1, Graph.getNodeID node2)
                end
            fun addInterference (flowNode, (graph, moves)) =
                let val {def, use, ismove} = MG.Graph.nodeInfo flowNode
                    val (liveIn, liveOut) = valOf (Table.look (liveMap, flowNode))
                    fun foldDef (def, graph) =
                        foldl
                        (fn (lo, graph) => addEdge (graph, def, lo))
                        graph
                        (Temp.Set.listItems liveOut)
                    val graph = foldl foldDef graph def
                in
                    if ismove
                    then
                        let val def = List.hd def
                            val use = List.hd use
                            val graph = Graph.removeEdge' (graph, {from=def, to=use})
                            val graph = Graph.removeEdge' (graph, {from=use, to=def})
                        in (
                            graph,
                            (Graph.getNode (graph, def), Graph.getNode (graph, use))::moves
                        )
                        end
                    else (graph, moves)
                end
            val (graph, moves) = foldl addInterference (Graph.empty, []) postList
        in (
            IGRAPH {
                graph=graph,
                tnode=fn temp => Graph.getNode (graph, temp),
                moves=moves
            },
            fn node => Temp.Set.listItems (#2 (valOf (Table.look (liveMap, node))))
        )
        end

    fun show (outstream, IGRAPH {graph, tnode, moves}) = ()
end