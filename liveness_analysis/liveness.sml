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
        graph: unit Graph.graph,
        tnode: Temp.temp -> unit Graph.node,
        moves: (unit Graph.node * unit Graph.node) list
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

    fun interferenceGraph flowGraph = 
        let val postList = postOrderNodes flowGraph
            (* Debugging *)
            (* fun printNode node = print (
                Int.toString (MG.Graph.getNodeID node)
                ^ " - "
                ^ MG.stringify (MG.Graph.nodeInfo node)
                ^ "\n"
            )
            val () = app printNode postList *)

            (* Liveness analysis *)
            fun liveMapLook (m, n) =
                case Table.look (m, n)
                of  SOME v => v
                |   NONE => (Temp.Set.empty, Temp.Set.empty)
            fun updateLiveness first (node, (liveMap, changed)) =
                let val (oldLI, oldLO) = liveMapLook (liveMap, node)
                    fun foldSucc (succ, liveOut) =
                        case Table.look (liveMap, succ)
                        of  SOME (succLI, succLO) => Temp.Set.union (liveOut, succLI)
                        |   NONE => liveOut
                    val newLO = foldl foldSucc oldLO (MG.Graph.succs' flowGraph node)
                    fun tempSetDelete (item, set) =
                        Temp.Set.delete (set, item)
                        handle NotFound => set
                in
                    if first orelse not (Temp.Set.equal (oldLO, newLO))
                    then
                        let val {def, use, ismove} = MG.Graph.nodeInfo node
                            val newLI = foldl tempSetDelete newLO def
                            val newLI = foldl Temp.Set.add' newLI use
                        in (
                            Table.enter (liveMap, node, (newLI, newLO)),
                            true
                        )
                        end
                    else (liveMap, changed)
                end
            fun iterToFixedPoint first init input =
                let val (result, changed) = foldl (updateLiveness first) (init, false) input
                in if changed then iterToFixedPoint false result input else result
                end
            val liveMap = iterToFixedPoint true Table.empty postList

            (* Build interference graph *)
            fun addNode (graph, temp) =
                (graph, Graph.getNode (graph, temp))
                handle Graph.NoSuchNode id => Graph.addNode' (graph, id, ())
            fun addEdge (graph, temp1, temp2) =
                let val (graph, node1) = addNode (graph, temp1)
                    val (graph, node2) = addNode (graph, temp2)
                in Graph.doubleEdge (graph, Graph.getNodeID node1, Graph.getNodeID node2)
                end
            fun addInterference (flowNode, (graph, moves)) =
                let val {def, use, ismove} = MG.Graph.nodeInfo flowNode
                    val (liveIn, liveOut) = liveMapLook (liveMap, flowNode)
                    val outList = Temp.Set.listItems liveOut
                    fun foldDef (def, graph) =
                        foldl
                        (fn (out, graph) => addEdge (graph, def, out))
                        graph
                        outList
                    val graph = foldl foldDef graph def
                in
                    if ismove
                    then
                        let val def = List.hd def
                            val use = List.hd use
                            val graph = Graph.removeEdge' (graph, {from=def, to=use})
                            val graph = Graph.removeEdge' (graph, {from=use, to=def})
                            val (graph, defNode) = addNode (graph, def)
                            val (graph, useNode) = addNode (graph, use)
                        in (
                            graph,
                            (defNode, useNode)::moves
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

    fun show (outstream, IGRAPH {graph, tnode, moves}) =
        let fun stringify (nid, _) = Temp.makestring nid
            val stringifyNode = (Temp.makestring o Graph.getNodeID)
            fun writeln x = TextIO.output (outstream, x ^ "\n")
            fun writeMove (t1, t2) =
                writeln ("  " ^ stringifyNode t1 ^ "<-" ^ stringifyNode t2)
        in
            Graph.writeGraph outstream stringify true graph;
            writeln "Moves:";
            app writeMove moves
        end
end