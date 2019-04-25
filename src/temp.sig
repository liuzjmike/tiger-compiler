signature TEMP = 
sig
    eqtype temp
    val newtemp : unit -> temp
    val compare : temp * temp -> order
    val makestring : temp -> string
    structure Set : ORD_SET sharing type Set.Key.ord_key = temp
    structure Map : ORD_MAP sharing type Map.Key.ord_key = temp
    val setDelete : Set.set * Set.item -> Set.set

    type label = Symbol.symbol
    val newlabel : unit -> label
    val namedlabel : string -> label
end

