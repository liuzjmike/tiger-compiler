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

  fun rewrite (instrs, frame, spillSet) =
    let
      fun makeMove x = List.rev (C.codegen frame (Tree.MOVE x))
      fun fromStack (temp, mem) = makeMove (Tree.TEMP temp, mem)
      fun toStack (temp, mem) = makeMove (mem, Tree.TEMP temp)
      fun foldTemp makeMove (temp, (moves, memMap)) =
        if Temp.Set.member (spillSet, temp)
        then
          case Temp.Map.find (memMap, temp) of
            SOME mem => (
              makeMove (temp, mem) @ moves,
              memMap
            )
          | NONE =>
            let
              val mem = Frame.exp (Frame.allocLocal frame true) (Tree.TEMP Frame.FP)
            in (
              makeMove (temp, mem) @ moves,
              Temp.Map.insert (memMap, temp, mem)
            )
            end
        else (moves, memMap)
      fun foldInstr (instr as Assem.OPER {assem, dst, src, jump}, (instrs, memMap)) =
        let
          val (loads, memMap) = foldl (foldTemp fromStack) ([], memMap) src
          val (saves, memMap) = foldl (foldTemp toStack) ([], memMap) dst
        in (saves @ instr :: loads @ instrs, memMap)
        end
      |   foldInstr (instr as Assem.MOVE {assem, dst, src}, (instrs, memMap)) =
        let
          val (loads, memMap) = foldTemp fromStack (src, ([], memMap))
          val (saves, memMap) = foldTemp toStack (dst, ([], memMap))
        in (saves @ instr :: loads @ instrs, memMap)
        end
      |   foldInstr (instr, (instrs, memMap)) = (instr::instrs, memMap)
      val (instrs', memMap) = foldl foldInstr ([], Temp.Map.empty) instrs
    in List.rev instrs'
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
        let
          val costMap =
            Temp.Set.foldl
            (fn (t, m) => Temp.Map.insert (m, t, Real.posInf))
            costMap spillSet
          val instrs' = rewrite (instrs, frame, spillSet)
        in alloc' (instrs', frame, costMap)
        end
    end

  (* TODO: better initial spill cost *)
  fun alloc (instrs, frame) = alloc' (instrs, frame, Temp.Map.empty)
end
