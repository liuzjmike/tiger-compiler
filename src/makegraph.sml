structure MakeGraph :>
sig
    structure Graph: FUNCGRAPH
    type nodeinfo = {def: Temp.temp list, use: Temp.temp list, ismove: bool}
    type node = nodeinfo Graph.node
    type graph = nodeinfo Graph.graph
    structure NodeMap: ORD_MAP where type Key.ord_key = node

    (* Returns a flow graph with only reachable instructions, the reachable
    instructions, a list of nodes in instruction order and a list a nodes in
    DFS post order *)
    val instrs2graph: Assem.instr list -> graph * Assem.instr list * node list * node list
    val stringify: nodeinfo -> string
    val show: TextIO.outstream * graph -> unit
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
    type node = nodeinfo Graph.node
    type graph = nodeinfo Graph.graph

    structure NodeOrd =
    struct 
        type ord_key = node
        fun compare (n1, n2) =
            Int.compare (Graph.getNodeID n1, Graph.getNodeID n2)
    end
    structure NodeMap = SplayMapFn (NodeOrd)
    structure Set = IntBinarySet

    datatype labelnode = Unknown of Graph.nodeID list
                       | Node of Graph.nodeID

    fun postOrderDFS (node, graph, visited, result) =
        let val id = Graph.getNodeID node
        in
            if Set.member (visited, id)
            then (result, visited)
            else
                let val visited = Set.add (visited, id)
                    val result = id::result
                    fun foldSucc (succ, (result, visited)) =
                        postOrderDFS (succ, graph, visited, result)
                in Graph.foldSuccs' graph foldSucc (result, visited) node
                end
        end

    fun instrs2graph instrList =
        let 
            (* Build flow graph *)
            val dummy = ~1
            val graph = Graph.addNode (
                Graph.empty, dummy,
                {def=[], use=[], ismove=false}
            )
            fun newNode (graph, id, def, use, ismove) =
                Graph.addNode (
                    graph, id,
                    {def=def, use=use, ismove=ismove}
                )

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

            (* Remove unreachable instructions *)
            val (postList, visited) =
                postOrderDFS (Graph.getNode (graph, 0), graph, Set.empty, [])
            (* Debugging *)
            (* fun printNode node = print (
                Int.toString (MG.Graph.getNodeID node)
                ^ " - "
                ^ MG.stringify (MG.Graph.nodeInfo node)
                ^ "\n"
            )
            val () = app printNode postList *)
            fun foldInstr ((instr, node), (instrs, graph)) =
                let val id = Graph.getNodeID node
                in
                    if Set.member (visited, id)
                    then (instr::instrs, graph)
                    else (instrs, Graph.removeNode (graph, id))
                end
            val (instrs, graph) =
                foldl foldInstr ([], graph) (ListPair.zipEq (instrList, Graph.nodes graph))
        in (
            graph, List.rev instrs, Graph.nodes graph,
            map (fn id => Graph.getNode (graph, id)) postList
        )
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
