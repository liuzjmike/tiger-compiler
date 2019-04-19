functor RegAlloc (F: FRAME) : REG_ALLOC =
struct
  structure C = Color (F)
  structure Frame = F

  type allocation = Frame.register Temp.Map.map

  fun defaultSpillCost x = 1.0;

  fun printAllocation(allocations: allocation, temps: Temp.temp list) =
    let
      fun printTemp(temp) =
        case Temp.Map.find(allocations, temp) of
          NONE => print("Cannot allocate: " ^ Temp.makestring(temp) ^ "\n")
        | SOME x => print(Temp.makestring temp  ^ " -> " ^ x ^ "\n")
    in
      app printTemp temps
    end

  fun alloc (instrs : Assem.instr list, frame: Frame.frame) =
    let
      val (fgraph, instrs, fnodes, postNodes) = MakeGraph.instrs2graph instrs
      val (igraph, _) = Liveness.interferenceGraph fgraph postNodes

      val (allocation, spills) = C.color {
        interference=igraph,
        initial=F.tempMap,
        spillCost=defaultSpillCost,
        registers=F.registers
      }
    in
      (instrs, allocation)
    end
end
