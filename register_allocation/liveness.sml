structure Liveness :> LIVENESS =
struct
    structure MG = MakeGraph
    structure Graph = FuncGraph (
        struct
            type ord_key = Temp.temp
            val compare = Temp.compare
        end
    )

    datatype igraph = IGRAPH of {
        graph: unit Graph.graph,
        moves: unit Graph.graph
    }

    fun interferenceGraph flowGraph instrs =
        let
            (* Liveness analysis *)
            fun liveMapLook (m, n) =
                case MG.NodeMap.find (m, n)
                of  SOME v => v
                |   NONE => (Temp.Set.empty, Temp.Set.empty)
            fun updateLiveness first (node, (liveMap, changed)) =
                let val (oldLI, oldLO) = liveMapLook (liveMap, node)
                    fun foldSucc (succ, liveOut) =
                        case MG.NodeMap.find (liveMap, succ)
                        of  SOME (succLI, succLO) => Temp.Set.union (liveOut, succLI)
                        |   NONE => liveOut
                    val newLO = MG.Graph.foldSuccs' flowGraph foldSucc oldLO node
                in
                    if first orelse not (Temp.Set.equal (oldLO, newLO))
                    then
                        let val {def, use, ismove} = MG.Graph.nodeInfo node
                            val newLI =
                                foldl (fn (v, set) => Temp.setDelete (set, v))
                                newLO def
                            val newLI = foldl Temp.Set.add' newLI use
                        in (
                            MG.NodeMap.insert (liveMap, node, (newLI, newLO)),
                            true
                        )
                        end
                    else (liveMap, changed)
                end
            fun iterToFixedPoint first init input =
                let val (result, changed) = foldl (updateLiveness first) (init, false) input
                in if changed then iterToFixedPoint false result input else result
                end
            val liveMap = iterToFixedPoint true MG.NodeMap.empty instrs

            (* Build interference graph *)
            fun addInterference (flowNode, (graph, moves)) =
                let val {def, use, ismove} = MG.Graph.nodeInfo flowNode
                    val (liveIn, liveOut) = liveMapLook (liveMap, flowNode)
                    (* FIXME: add def to outList? *)
                    val outList = Temp.Set.listItems liveOut
                    fun foldDef (def, graph) =
                        let val graph = Graph.addNewNode (graph, def, ())
                            fun addEdge (out, graph) =
                                Graph.doubleEdge (
                                    Graph.addNewNode (graph, out, ()),
                                    def, out
                                )
                        in foldl addEdge graph outList
                        end
                    val graph = foldl foldDef graph def
                in
                    if ismove
                    then
                        let val def = List.hd def
                            val use = List.hd use
                            val graph = Graph.removeEdge'' (graph, {from=def, to=use})
                            val graph = Graph.removeEdge'' (graph, {from=use, to=def})
                            val moves = Graph.addNewNode (moves, def, ())
                            val moves = Graph.addNewNode (moves, use, ())
                        in (
                            graph,
                            Graph.doubleEdge (moves, def, use)
                        )
                        end
                    else (graph, moves)
                end
            val (graph, moves) = foldl addInterference (Graph.empty, Graph.empty) instrs
        in (
            IGRAPH {graph=graph, moves=moves},
            fn node => Temp.Set.listItems (#2 (valOf (MG.NodeMap.find (liveMap, node))))
        )
        end

    fun show (outstream, IGRAPH {graph, moves}) =
        let fun stringify (nid, _) = Temp.makestring nid
            fun writeln x = TextIO.output (outstream, x ^ "\n")
            fun writeMove (t1, t2) =
                writeln ("  " ^ Temp.makestring t1 ^ "<-" ^ Temp.makestring t2)
        in
            Graph.writeGraph outstream stringify true graph;
            TextIO.output (outstream, "Moves\n");
            Graph.writeGraph outstream stringify true moves
        end
end