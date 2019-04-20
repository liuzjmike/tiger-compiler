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
          NONE => print ("Cannot allocate: " ^ Temp.makestring(temp) ^ "\n")
        | SOME x => print (Temp.makestring temp  ^ " -> " ^ x ^ "\n")
    in
      app printTemp temps
    end

  fun getRegister allocation temp =
    case Temp.Map.find (allocation, temp) of
      SOME name => name
    | NONE =>
      let val name = Temp.makestring temp
      in print (name ^ " not allocated\n"); name
      end

  fun alloc (instrs, frame) =
    let
      val (fgraph, instrs, fnodes, postNodes) = MakeGraph.instrs2graph instrs
      val (igraph, _) = Liveness.interferenceGraph fgraph postNodes
      val (allocation, spills) = C.color {
        interference=igraph,
        initial=F.tempMap,
        spillCost=defaultSpillCost,
        registers=F.registers
      }
      (* val () = Liveness.show (TextIO.stdOut, igraph) *)
      val getRegister = getRegister allocation
      fun filterInstr (Assem.MOVE {assem, dst, src}) =
          getRegister dst <> getRegister src
      |   filterInstr _ = true
    in (List.filter filterInstr instrs, allocation)
    end
end
