signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Map.map
    val color : {
        interference: Liveness.igraph,
        initial: allocation,
        spillCost: Temp.temp -> real,
        registers: Frame.register list
    } -> allocation * Temp.Set.set
end