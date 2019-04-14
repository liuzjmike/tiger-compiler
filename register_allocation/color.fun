functor Color (F : FRAME) : COLOR =
struct
    structure Frame = F
    structure G = Liveness.Graph
    type allocation = Frame.register Temp.Map.map

    (* fun buildWorklist (igraph, moves, initial) =
        let val nReg = Temp.Map.numItems initial
            fun foldNode (node, (spill, frozen, simplify)) =
                let val temp = G.getNodeID node
                in
                    case Temp.Map.find (initial, temp)
                    of  SOME _ => (spill, frozen, simplify)
                    |   NONE =>
                        if G.outDegree node < nReg
                        then
                            case G.getNode' (moves, temp)
                            of  SOME _ => (
                                spill, Temp.Set.add (frozen, temp),
                                simplify
                            )
                            |   NONE => (
                                spill, frozen,
                                Temp.Set.add (simplify, temp)
                            )
                        else (
                            Temp.Set.add (spill, temp),
                            frozen, simplify
                        )
                end
            in
                G.foldNodes foldNode
                (Temp.Set.empty, Temp.Set.empty, Temp.Set.empty)
                igraph
            end *)

    fun color {
        interference=Liveness.IGRAPH {graph, moves},
        initial, spillCost, registers
    } =
        let val nReg = Temp.Map.numItems initial

            (* Build worklists *)
            fun foldNode (node, (simplify, frozen, spill)) =
                let val temp = G.getNodeID node
                in
                    case Temp.Map.find (initial, temp)
                    of  SOME _ => (simplify, frozen, spill)
                    |   NONE =>
                        if G.outDegree node < nReg
                        then
                            case G.getNode' (moves, temp)
                            of  SOME _ => (
                                simplify, Temp.Set.add (frozen, temp),
                                spill
                            )
                            |   NONE => (
                                Temp.Set.add (simplify, temp),
                                frozen, spill
                            )
                        else (
                            simplify, frozen,
                            Temp.Set.add (spill, temp)
                        )
                end
            val (spillSet, frozenSet, simplifySet) =
                G.foldNodes foldNode
                (Temp.Set.empty, Temp.Set.empty, Temp.Set.empty)
                graph

            (* Simplify *)
            fun getItem set = valOf (Temp.Set.find (fn _ => true) set)
            fun enableMoves (temp, (activeMoves, pendingMoves)) =
                case G.getNode' (pendingMoves, temp)
                of  NONE => (activeMoves, pendingMoves)
                |   SOME node =>
                    let val activeMoves = G.addNewNode (activeMoves, temp, ())
                        fun foldAdj (t, moves) =
                            G.doubleEdge (
                                G.addNewNode (moves, t, ()),
                                temp, t
                            )
                        val activeMoves = G.foldSuccs foldAdj activeMoves node
                    in (activeMoves, G.removeNode (pendingMoves, temp))
                    end
            fun moveRelated (temp, activeMoves, pendingMoves) =
                case (G.getNode' (activeMoves, temp), G.getNode' (pendingMoves, temp))
                of  (NONE, NONE) => false
                |   _ => true
            fun adjustWorklist (node, (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)) =
                if G.outDegree node = nReg - 1
                then
                    let val temp = G.getNodeID node
                        val (activeMoves, pendingMoves) = enableMoves (temp, (activeMoves, pendingMoves))
                        val (activeMoves, pendingMoves) = G.foldSuccs enableMoves (activeMoves, pendingMoves) node
                        val spillSet = Temp.Set.delete (spillSet, temp)
                        val (simplifySet, frozenSet) =
                            if moveRelated (temp, activeMoves, pendingMoves)
                            then (simplifySet, Temp.Set.add (frozenSet, temp))
                            else (Temp.Set.add (simplifySet, temp), frozenSet)
                    in (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
                    end
                else (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
            fun simplify (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
                let val temp = getItem simplifySet
                    val node = G.getNode (igraph, temp)
                    val igraph = G.removeNode (igraph, temp)
                    (* val moves = G.removeNode' (moves, temp) *)
                    val simplifySet = Temp.Set.delete (simplifySet, temp)
                    val (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
                        G.foldSuccs' igraph adjustWorklist
                        (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) node
                in (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
                end

            (* Unfreeze *)
            fun unfreeze (igraph, pendingMoves, simplifySet, frozenSet, spillSet) =
                let val temp = getItem frozenSet
                    val simplifySet = Temp.Set.add (simplifySet, temp)
                    val frozenSet = Temp.Set.delete (frozenSet, temp)
                    val adj = G.succs (G.getNode (pendingMoves, temp))
                    val pendingMoves = G.removeNode (pendingMoves, temp)
                    fun foldAdj (t, (pendingMoves, simplifySet, frozenSet)) =
                        if G.outDegree (G.getNode (pendingMoves, t)) = 0
                        then
                            let val pendingMoves = G.removeNode (pendingMoves, t)
                            in
                                if G.outDegree (G.getNode (igraph, t)) < nReg
                                then (
                                    pendingMoves,
                                    Temp.Set.add (simplifySet, t),
                                    Temp.Set.delete (frozenSet, t)
                                )
                                else (pendingMoves, simplifySet, frozenSet)
                            end
                        else (pendingMoves, simplifySet, frozenSet)
                    val (pendingMoves, simplifySet, frozenSet) =
                        foldl foldAdj (pendingMoves, simplifySet, frozenSet) adj
                in (igraph, pendingMoves, simplifySet, frozenSet, spillSet)
                end
            
        in
            (Frame.tempMap, [])
        end
end