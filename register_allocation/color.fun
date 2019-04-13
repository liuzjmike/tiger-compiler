functor Color (F : FRAME) : COLOR =
struct
    structure Frame = F
    structure L = Liveness
    type allocation = Frame.register Temp.Map.map

    fun color {
        interference=Liveness.IGRAPH {graph, moves},
        initial, spillCost, registers
    } =
        let val nReg = Temp.Map.numItems initial
            fun foldNode (node, (spill, freeze, simplify)) =
                let val temp = L.Graph.getNodeID node
                in
                    case Temp.Map.find (initial, temp)
                    of  SOME _ => (spill, freeze, simplify)
                    |   NONE =>
                        if L.Graph.outDegree node < nReg
                        then
                            case L.Graph.getNode' (moves, temp)
                            of  SOME _ => (
                                spill, Temp.Set.add (freeze, temp),
                                simplify
                            )
                            |   NONE => (
                                spill, freeze,
                                Temp.Set.add (simplify, temp)
                            )
                        else (
                            Temp.Set.add (spill, temp),
                            freeze, simplify
                        )
                end
            val (spill, freeze, simplify) =
                L.Graph.foldNodes foldNode
                (Temp.Set.empty, Temp.Set.empty, Temp.Set.empty)
                graph
        in
            (Frame.tempMap, [])
        end
end