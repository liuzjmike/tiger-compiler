functor Color (F : FRAME) : COLOR =
struct
  structure Frame = F
  structure G = Liveness.Graph
  type allocation = Frame.register Temp.Map.map

  fun color {
    interference=Liveness.IGRAPH {graph, moves},
    initial, spillCost, registers
  } =
    let
      val nReg = Temp.Map.numItems initial

      (* Build worklists *)
      fun foldNode (node, (simplify, frozen, spill)) =
        let val temp = G.getNodeID node
        in
          case Temp.Map.find (initial, temp) of
            SOME _ => (simplify, frozen, spill)
          | NONE =>
            if G.outDegree node < nReg
            then
              case G.getNode' (moves, temp) of
                SOME _ => (
                  simplify, Temp.Set.add (frozen, temp),
                  spill
                )
              | NONE => (
                  Temp.Set.add (simplify, temp),
                  frozen, spill
                )
            else (
              simplify, frozen,
              Temp.Set.add (spill, temp)
            )
        end
      val (spillSet, frozenSet, simplifySet) =
        G.foldNodes foldNode
        (Temp.Set.empty, Temp.Set.empty, Temp.Set.empty)
        graph

      (* Simplify *)
      fun getItem set = valOf (Temp.Set.find (fn _ => true) set)
      fun enableMoves (temp, (activeMoves, pendingMoves)) =
        case G.getNode' (pendingMoves, temp) of
          NONE => (activeMoves, pendingMoves)
        | SOME node =>
          let
            val activeMoves = G.addNewNode (activeMoves, temp, ())
            fun foldAdj (t, moves) =
              G.doubleEdge (
                G.addNewNode (moves, t, ()),
                temp, t
              )
            val activeMoves = G.foldSuccs foldAdj activeMoves node
          in (activeMoves, G.removeNode (pendingMoves, temp))
          end
      fun moveRelated (temp, activeMoves, pendingMoves) =
        case (G.getNode' (activeMoves, temp), G.getNode' (pendingMoves, temp)) of
          (NONE, NONE) => false
        | _ => true
      fun adjustWorklist (node, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          fun f (node, (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)) =
            if G.outDegree node = nReg - 1
            then
              let
                val temp = G.getNodeID node
                val (activeMoves, pendingMoves) = enableMoves (temp, (activeMoves, pendingMoves))
                val (activeMoves, pendingMoves) = G.foldSuccs enableMoves (activeMoves, pendingMoves) node
                val spillSet = Temp.Set.delete (spillSet, temp)
                val (simplifySet, frozenSet) =
                  if moveRelated (temp, activeMoves, pendingMoves)
                  then (simplifySet, Temp.Set.add (frozenSet, temp))
                  else (Temp.Set.add (simplifySet, temp), frozenSet)
              in (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
              end
            else (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        in
          G.foldSuccs' igraph f
          (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) node
        end
      fun simplify (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val temp = getItem simplifySet
          val node = G.getNode (igraph, temp)
          val igraph = G.removeNode (igraph, temp)
          (* val moves = G.removeNode' (moves, temp) *)
          val simplifySet = Temp.Set.delete (simplifySet, temp)
          val (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
            adjustWorklist (node, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        in (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        end

      (* Coalesce *)
      fun precolored temp =
        case Temp.Map.find (initial, temp) of
          SOME _ => true
        | NONE => false
      fun removeIsolated (graph, id) =
        if G.outDegree (G.getNode (graph, id)) = 0
        then G.removeNode (graph, id)
        else graph
      fun frozenToSimplify (temp, igraph, activeMoves, pendingMoves, simplifySet, frozenSet) =
        if not (precolored temp)
        andalso not (moveRelated (temp, activeMoves, pendingMoves))
        andalso G.outDegree (G.getNode (igraph, temp)) < nReg
        then (Temp.Set.add (simplifySet, temp), Temp.Set.delete (frozenSet, temp))
        else (simplifySet, frozenSet)
      fun george (u, v, igraph) =
        let
          fun f (t, (ok, precoloredOK)) =
            let
              val node = G.getNode (igraph, t)
              val okHere =
                G.outDegree node < nReg
                orelse G.isAdjacent (node, u)
            in (
              ok andalso okHere,
              precoloredOK andalso (okHere orelse precolored t)
            )
            end
        in G.foldSuccs f (true, true) (G.getNode (igraph, v))
        end
      fun briggs (u, v, igraph) =
        let
          val u = G.getNode (igraph, u)
          val v = G.getNode (igraph, v)
          val neighbors = foldl Temp.Set.add' Temp.Set.empty (G.succs u)
          val neighbors = foldl Temp.Set.add' neighbors (G.succs v)
          fun f (t, cnt) =
            let
              val degree = G.outDegree (G.getNode (igraph, t))
              val degree =
                if G.isAdjacent (u, t) andalso G.isAdjacent (v, t)
                then degree - 1
                else degree
            in if degree < nReg then cnt else cnt + 1
            end
        in (Temp.Set.foldl f 0 neighbors) < nReg
        end
      fun combineNodes (graph, u, v) =
        let
          val graph =
            G.foldSuccs (fn (t, g) => G.doubleEdge (g, u, t))
            graph (G.getNode (graph, v))
        in G.removeNode (graph, v)
        end
      fun combine (u, v, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val node = G.getNode (igraph, v)
          val igraph = combineNodes (igraph, u, v)
          val activeMoves = combineNodes (activeMoves, u, v)
          val pendingMoves = combineNodes (pendingMoves, u, v)
          val frozenSet = Temp.setDelete (frozenSet, v)
          val spillSet = Temp.setDelete (spillSet, v)
          val (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
            adjustWorklist (node, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          val (frozenSet, spillSet) =
            if G.outDegree (G.getNode (igraph, u)) < nReg
            then (frozenSet, spillSet)
            else (Temp.setDelete (frozenSet, u), Temp.Set.add (spillSet, u))
          val (fronzenSet, simplifySet) = frozenToSimplify (
            u, igraph, activeMoves, pendingMoves, simplifySet, frozenSet
          )
        in (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        end
          
      fun coalesce (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val u = List.hd (G.nodes activeMoves)
          val v = valOf (G.oneSucc u)
          val u = G.getNodeID u
          val (u, v) = case Temp.Map.find (initial, v)
            of  SOME _ => (v, u)
            |   NONE => (u, v)
          val activeMoves = G.removeEdge (
            G.removeEdge (activeMoves, {from=u, to=v}),
            {from=v, to=u}
          )
          val activeMoves = removeIsolated (
            removeIsolated (activeMoves, u), v
          )
        in
          if precolored v orelse G.isAdjacent (G.getNode (igraph, u), v)
          then
            let
              val (simplifySet, frozenSet) =
                frozenToSimplify (u, igraph, activeMoves, pendingMoves, simplifySet, frozenSet)
              val (simplifySet, frozenSet) =
                frozenToSimplify (v, igraph, activeMoves, pendingMoves, simplifySet, frozenSet)
            in (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
            end
          else
            let
              val uPrecolored = precolored u
              val (georgeOK, precoloredOK) = george (u, v, igraph)
              val briggsOK = briggs (u, v, igraph)
            in
              if uPrecolored andalso precoloredOK
              orelse not uPrecolored andalso (georgeOK orelse briggsOK)
              then combine (
                u, v, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet
              )
              else if #1 (george (v, u, igraph))
              then combine (
                v, u, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet
              )
              else
                let val pendingMoves = G.doubleEdge (
                  G.addNewNode (G.addNewNode (pendingMoves, v, ()), u, ()),
                  u, v
                )
                in (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
                end
            end
        end

      (* Unfreeze *)
      fun unfreeze (igraph, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val temp = getItem frozenSet
          val simplifySet = Temp.Set.add (simplifySet, temp)
          val frozenSet = Temp.Set.delete (frozenSet, temp)
          val adj = G.succs (G.getNode (pendingMoves, temp))
          val pendingMoves = G.removeNode (pendingMoves, temp)
          fun foldAdj (t, (pendingMoves, simplifySet, frozenSet)) =
            if G.outDegree (G.getNode (pendingMoves, t)) = 0
            then
              (* Remove t from pendingMoves if it is not move-related anymore *)
              let val pendingMoves = G.removeNode (pendingMoves, t)
              in
                (* If t is in frozenSet move it to simplifySet *)
                if G.outDegree (G.getNode (igraph, t)) < nReg
                then (
                  pendingMoves,
                  Temp.Set.add (simplifySet, t),
                  Temp.Set.delete (frozenSet, t)
                )
                else (pendingMoves, simplifySet, frozenSet)
              end
            else (pendingMoves, simplifySet, frozenSet)
          val (pendingMoves, simplifySet, frozenSet) =
            foldl foldAdj (pendingMoves, simplifySet, frozenSet) adj
        in (igraph, pendingMoves, simplifySet, frozenSet, spillSet)
        end
      
    in
      (Frame.tempMap, [])
    end
end