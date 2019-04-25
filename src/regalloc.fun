functor RegAlloc (C : CODEGEN) : REG_ALLOC =
struct
  structure Frame = C.Frame
  structure Color = Color (Frame)

  type allocation = Frame.register Temp.Map.map

  fun defaultSpillCost x = 1.0;

  fun spillCost costMap temp =
    case Temp.Map.find (costMap, temp) of
      SOME cost => cost
    | NONE => 1.0

  fun getRegister allocation temp =
    case Temp.Map.find (allocation, temp) of
      SOME name => name
    | NONE =>
      ErrorMsg.impossible (Temp.makestring temp ^ " not allocated\n")

  fun rewrite (instrs, frame, spillSet, costMap) =
    let
      fun makeMove x = List.rev (C.codegen frame (Tree.MOVE x))
      fun fromStack (temp, mem) = makeMove (Tree.TEMP temp, mem)
      fun toStack (temp, mem) = makeMove (mem, Tree.TEMP temp)
      fun foldTemp makeMove (temp, (temps, moves, memMap, costMap)) =
        if Temp.Set.member (spillSet, temp)
        then
          let
            val t = Temp.newtemp ()
            val costMap = Temp.Map.insert (costMap, t, Real.posInf)
          in
            case Temp.Map.find (memMap, temp) of
              SOME mem => (
                t::temps,
                makeMove (t, mem) @ moves,
                memMap,
                costMap
              )
            | NONE =>
              let
                val mem = Frame.exp (Frame.allocLocal frame true) (Tree.TEMP Frame.FP)
              in (
                t::temps,
                makeMove (t, mem) @ moves,
                Temp.Map.insert (memMap, temp, mem),
                costMap
              )
              end
          end
        else (temp::temps, moves, memMap, costMap)
      fun foldInstr (Assem.OPER {assem, dst, src, jump}, (instrs, memMap, costMap)) =
        let
          val (src, loads, memMap, costMap) =
            foldl (foldTemp fromStack) ([], [], memMap, costMap) src
          val (dst, saves, memMap, costMap) =
            foldl (foldTemp toStack) ([], [], memMap, costMap) dst
          val instr = Assem.OPER {assem=assem, dst=List.rev dst, src=List.rev src, jump=jump}
        in (saves @ instr :: loads @ instrs, memMap, costMap)
        end
      |   foldInstr (Assem.MOVE {assem, dst, src}, (instrs, memMap, costMap)) =
        let
          val (src, loads, memMap, costMap) =
            foldTemp fromStack (src, ([], [], memMap, costMap))
          val (dst, saves, memMap, costMap) =
            foldTemp toStack (dst, ([], [], memMap, costMap))
          val instr = Assem.MOVE {assem=assem, dst=List.hd dst, src=List.hd src}
        in (saves @ instr :: loads @ instrs, memMap, costMap)
        end
      |   foldInstr (instr, (instrs, memMap, costMap)) = (instr::instrs, memMap, costMap)
      val (instrs', memMap, costMap) = foldl foldInstr ([], Temp.Map.empty, costMap) instrs
    in (List.rev instrs', costMap)
    end

  fun alloc' (instrs, frame, costMap) =
    let
      val (fgraph, instrs, fnodes, postNodes) = MakeGraph.instrs2graph instrs
      val (igraph, _) = Liveness.interferenceGraph fgraph postNodes
      val (allocation, spillSet) = Color.color {
        interference=igraph,
        initial=Frame.tempMap,
        spillCost=spillCost costMap,
        registers=Frame.registers
      }
    in
      if Temp.Set.numItems spillSet = 0
      then
        let
          val getRegister = getRegister allocation
          fun filterInstr (Assem.MOVE {assem, dst, src}) =
              getRegister dst <> getRegister src
          |   filterInstr _ = true
        in (List.filter filterInstr instrs, allocation)
        end
      else
        let val (instrs', costMap) = rewrite (instrs, frame, spillSet, costMap)
        in alloc' (instrs', frame, costMap)
        end
    end

  (* TODO: better initial spill cost *)
  fun alloc (instrs, frame) = alloc' (instrs, frame, Temp.Map.empty)
end
