signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Map.map
    val color : {
        interference: Liveness.igraph,
        initial: allocation,
        spillCost: Temp.temp -> int,
        registers: Frame.register list
    } -> allocation * Temp.temp list
end