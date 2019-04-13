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
            (* fun moveMapFind (moveMap, temp) =
                case Temp.Map.Find (moveMap, temp)
                of  SOME s => s
                |   NONE => Temp.Set.empty
            fun foldMove ((dst, src), moveMap) =
                if dst = src then moveMap
                else
                    let val dstAdj
                        val m = Temp.Map.insert (moveMap, dst, Temp.Set.add (moveMapFind)) *)
            fun foldNode (node, (spill, freeze, simplify)) =
                let val temp = L.Graph.getNodeID node
                in
                    case Temp.Map.find (initial, temp)
                    of  SOME _ => (spill, freeze, simplify)
                    |   NONE =>
                        if L.Graph.outDegree node >= nReg
                        then (
                            Temp.Set.add (spill, temp),
                            freeze, simplify
                        )
                        else (spill, freeze, simplify)
                end
        in
            (Frame.tempMap, [])
        end
end