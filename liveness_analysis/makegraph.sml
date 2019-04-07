structure MakeGraph :
sig
    structure Graph: FUNCGRAPH
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

    fun instrs2graph instrList =
        let val dummyNode = ~1
            val graph = Graph.addNode (
                Graph.empty, dummyNode,
                {def=[], use=[], ismove=false}
            )
            val nextID = ref 0
            fun newNode (graph, def, use, ismove) =
                let val id = !nextID
                    val (graph, node) = Graph.addNode' (
                        graph, id, {def=def, use=use, ismove=ismove}
                    )
                in
                    nextID := id + 1;
                    (graph, id, node)
                end
            fun addLabelNode (graph, label, labelMap) =
                case Symbol.look (labelMap, label)
                of  SOME node => (graph, Graph.getNodeID node, node, labelMap)
                |   NONE =>
                    let val (graph, id, node) = newNode (graph, [], [], false)
                    in (graph, id, node, Symbol.enter (labelMap, label, node))
                    end

            (* Given an instruction, a graph and a map from label to node,
            adds a node associated with the instruction to the graph and
            returns the updated graph, id of the node, the node itself,
            the updated map and the id of the node that is the parent of
            the next instruction *)
            fun addNode (
                A.OPER {assem, dst, src, jump=NONE},
                graph, labelMap
            ) =
                let val (graph, id, node) = newNode (graph, dst, src, false)
                in (graph, id, node, labelMap, id)
                end
            |   addNode (
                A.OPER {assem, dst, src, jump=SOME jump},
                graph, labelMap
            ) =
                let val (graph, id, node) = newNode (graph, dst, src, false)
                    fun foldJumpLabel (label, (graph, jumpNodes, labelMap)) =
                        let val (graph, id, node, labelMap) =
                                addLabelNode (graph, label, labelMap)
                        in (graph, id::jumpNodes, labelMap)
                        end
                    val (graph, jumpNodes, labelMap) =
                        foldl foldJumpLabel (graph, [], labelMap) jump
                    fun foldJumpNode (jumpNode, graph) =
                        Graph.addEdge (graph, {
                            from=id,
                            to=jumpNode
                        })
                    val graph = foldl foldJumpNode graph jumpNodes
                in (graph, id, node, labelMap, dummyNode)
                end
            |   addNode (A.LABEL {assem, lab}, graph, labelMap) =
                let val (graph, id, node, labelMap) = addLabelNode (graph, lab, labelMap)
                in (graph, id, node, labelMap, id)
                end
            |   addNode (A.MOVE {assem, dst, src}, graph, labelMap) =
                let val (graph, id, node) = newNode (graph, [dst], [src], true)
                in (graph, id, node, labelMap, id)
                end
            fun foldInstr (instr, (graph, nodeList, parent, labelMap)) =
                let val (graph, id, node, labelMap, nextParent) =
                        addNode (instr, graph, labelMap)
                    val graph = Graph.addEdge (
                        graph, {from=parent, to=id}
                    )
                in (graph, node::nodeList, nextParent, labelMap)
                end
            val (graph, nodeList, lastNode, labelMap) =
                foldl foldInstr (graph, [], dummyNode, Symbol.empty) instrList
            val graph = Graph.removeNode (graph, dummyNode)
        in (graph, List.rev nodeList)
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
