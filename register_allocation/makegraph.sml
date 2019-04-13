structure MakeGraph :>
sig
    structure Graph: FUNCGRAPH where type nodeID = int
    type nodeinfo = {def: Temp.temp list, use: Temp.temp list, ismove: bool}
    val instrs2graph: Assem.instr list -> nodeinfo Graph.graph * nodeinfo Graph.node list
    val stringify: nodeinfo -> string
    val show: TextIO.outstream * nodeinfo Graph.graph -> unit
end =
struct
    structure A = Assem
    structure Graph = FuncGraph (
        struct
            type ord_key = int
            val compare = Int.compare
        end
    )
    type nodeinfo = {def: Temp.temp list, use: Temp.temp list, ismove: bool}
    datatype labelnode = Unknown of Graph.nodeID list
                       | Node of Graph.nodeID

    fun instrs2graph instrList =
        let val dummy = ~1
            val graph = Graph.addNode (
                Graph.empty, dummy,
                {def=[], use=[], ismove=false}
            )
            fun newNode (graph, id, def, use, ismove) =
                Graph.addNode (graph, id, {def=def, use=use, ismove=ismove})

            (* Given an instruction, a graph and a map from label to node,
            adds a node associated with the instruction to the graph and
            returns the updated graph, id of the node, the node itself,
            the updated map and the id of the node that is the parent of
            the next instruction *)
            fun addNode (
                A.OPER {assem, dst, src, jump=NONE},
                graph, id, labelMap
            ) =
                let val graph = newNode (graph, id, dst, src, false)
                in (graph, labelMap, id)
                end
            |   addNode (
                A.OPER {assem, dst, src, jump=SOME jump},
                graph, id, labelMap
            ) =
                let val graph = newNode (graph, id, dst, src, false)
                    fun foldJump (label, (graph, labelMap)) =
                        case Symbol.look (labelMap, label)
                        of  NONE => (graph, Symbol.enter (labelMap, label, Unknown [id]))
                        |   SOME (Unknown list) =>
                            (graph, Symbol.enter (labelMap, label, Unknown (id::list)))
                        |   SOME (Node node) =>
                            (Graph.addEdge (graph, {from=id, to=node}), labelMap)
                    val (graph, labelMap) =
                        foldl foldJump (graph, labelMap) jump
                in (graph, labelMap, dummy)
                end
            |   addNode (A.LABEL {assem, lab}, graph, id, labelMap) =
                let val graph = newNode (graph, id, [], [], false)
                    val (graph, labelMap) = case Symbol.look (labelMap, lab)
                        of  NONE => (graph, Symbol.enter (labelMap, lab, Node id))
                        |   SOME (Unknown list) =>
                            let fun addEdge (fromID, graph) =
                                Graph.addEdge (graph, {from=fromID, to=id})
                            in (foldl addEdge graph list, Symbol.enter (labelMap, lab, Node id))
                            end
                        |   SOME (Node node) => ErrorMsg.impossible "duplicate label"
                in (graph, labelMap, id)
                end
            |   addNode (A.MOVE {assem, dst, src}, graph, id, labelMap) =
                let val graph = newNode (graph, id, [dst], [src], true)
                in (graph, labelMap, id)
                end
            fun foldInstr (instr, (graph, id, parent, labelMap)) =
                let val (graph, labelMap, nextParent) =
                        addNode (instr, graph, id, labelMap)
                    val graph = Graph.addEdge (
                        graph, {from=parent, to=id}
                    )
                in (graph, id+1, nextParent, labelMap)
                end
            val (graph, id, lastNode, labelMap) =
                foldl foldInstr (graph, 0, dummy, Symbol.empty) instrList
            val graph = Graph.removeNode (graph, dummy)
        in (graph, Graph.nodes graph)
        end

    fun stringify {def, use, ismove} =
        let fun stringifyTemps [] ans = ans
            |   stringifyTemps [a] ans = ans ^ Temp.makestring a
            |   stringifyTemps (a::l) ans =
                stringifyTemps l (ans ^ Temp.makestring a ^ ", ")
        in 
            "def: [" ^ stringifyTemps def ""
            ^ "], use: [" ^ stringifyTemps use ""
            ^ "], ismove: " ^ (if ismove then "true" else "false")
        end

    fun show (outstream, graph) =
        let fun stringify' (nid, info) =
                Int.toString nid ^ " - " ^ stringify info
        in Graph.writeGraph outstream stringify' false graph
        end
end
