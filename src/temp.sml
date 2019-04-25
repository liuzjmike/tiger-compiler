(* make this an abstraction sometime *)
structure Temp :> TEMP =
struct
    type temp = int
    val temps = ref 100
    fun newtemp() = let val t = !temps in temps := t+1; t end
    val compare = Int.compare
    fun makestring t = "t" ^ Int.toString t

    structure TempOrd =
    struct 
        type ord_key = temp
        val compare = compare
    end
    structure Set = SplaySetFn(TempOrd)
    structure Map = SplayMapFn(TempOrd)

    fun setDelete (set, v) =
        Set.delete (set, v)
        handle LibBase.NotFound => set

    type label = Symbol.symbol

    local structure F = Format
        fun postinc x = let val i = !x in x := i+1; i end
        val labs = ref 0
    in
        fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
        val namedlabel = Symbol.symbol
    end

end
