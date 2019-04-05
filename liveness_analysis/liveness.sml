structure Liveness : LIVENESS =
struct
    structure MG = MakeGraph
    structure Graph = FuncGraph (
        struct
            type ord_key = int
            val compare = Int.compare
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
    type live_set = Temp.temp Temp.Table.table
    type live_map = live_set Table.table

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

    fun interferenceGraph (flowGraph, nodeList) = 
        let 
            (* Reorders nodeList into post-order *)
            fun foldNode1 (node, (list, visited)) =
                let val (result, visited) = postOrderDFS (node, flowGraph, visited, [])
                in (list @ result, visited)
                end
            val (postList, visited) = foldl foldNode1 ([], Table.empty) nodeList

            (* Liveness analysis *)
            fun updateLiveness (node, (liveMap, changed)) =
                let val (oldLI, oldLO) =
                        case Table.look (liveMap, node)
                        of  SOME v => v
                        |   NONE => (Temp.Table.empty, Temp.Table.empty)
                    fun foldSucc (succ, liveOut) =
                        case Table.look (liveMap, succ)
                        of  SOME (succLI, succLO) => Temp.Table.update (liveOut, succLI)
                        |   NONE => liveOut
                    val newLO = foldl foldSucc oldLO (MG.Graph.succs' flowGraph node)
                in
                    if Temp.Table.equal (oldLO, newLO)
                    then (liveMap, changed)
                    else
                        let val {def, use, ismove} = MG.Graph.nodeInfo node
                            val newLI = foldl Temp.Table.remove' newLO def
                            val newLI = foldl (fn (use, liveIn) => Temp.Table.enter (liveIn, use, use))
                                        newLI use
                        in (Table.enter (liveMap, node, (newLI, newLO)), true)
                        end
                end
            fun analyzeLiveness liveMap =
                let val (liveMap, changed) = foldl updateLiveness (liveMap, false) postList
                in if changed then analyzeLiveness liveMap else liveMap
                end
            val liveMap = analyzeLiveness Table.empty

            (* Build interference graph *)
            fun addNode (graph, temp) =
                (graph, Graph.getNode (graph, Temp.tempID temp))
                handle Graph.NoSuchNode id => Graph.addNode' (graph, id, temp)
            fun addEdge (graph, temp1, temp2) =
                let val (graph, node1) = addNode (graph, temp1)
                    val (graph, node2) = addNode (graph, temp2)
                in Graph.doubleEdge (graph, Graph.getNodeID node1, Graph.getNodeID node2)
                end
            fun foldNode2 (fnode, (graph, moves)) =
                let val {def, use, ismove} = MG.Graph.nodeInfo fnode
                    val (liveIn, liveOut) = valOf (Table.look (liveMap, fnode))
                    fun foldDef (def, graph) =
                        foldl
                        (fn (lo, graph) => addEdge (graph, def, lo))
                        graph
                        (Temp.Table.items liveOut)
                    val graph = foldl foldDef graph def
                in
                    if ismove
                    then
                        let val def = Temp.tempID (List.hd def)
                            val use = Temp.tempID (List.hd use)
                            val graph = Graph.removeEdge' (graph, {from=def, to=use})
                            val graph = Graph.removeEdge' (graph, {from=use, to=def})
                        in (
                            graph,
                            (Graph.getNode (graph, def), Graph.getNode (graph, use))::moves
                        )
                        end
                    else (graph, moves)
                end
            val (graph, moves) = foldl foldNode2 (Graph.empty, []) nodeList
        in (
            IGRAPH {
                graph=graph,
                tnode=fn temp => Graph.getNode (graph, Temp.tempID temp),
                moves=moves
            },
            fn node => Temp.Table.items (#2 (valOf (Table.look (liveMap, node))))
        )
        end

    fun show (outstream, IGRAPH {graph, tnode, moves}) = ()
end