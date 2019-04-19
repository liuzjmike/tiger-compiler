functor Color (F : FRAME) : COLOR =
struct
  structure Frame = F
  structure G = Liveness.Graph
  structure Set = SplaySetFn (
    struct
      type ord_key = string
      val compare = String.compare
    end
  )
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

      (* Simplify helpers *)
      fun getItem set = Temp.Set.find (fn _ => true) set
      (* Makes pending moves related to temp active *)
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
      (* Should be called after `node` is removed from `igraph`.
      If a neighbor of `node` beomes low-degree, moves it from `spillSet` to `simplifySet`
      or `forzenSet` and enables the moves related to itself and its neighbors. *)
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

      (* Coalesce helpers *)
      fun precolored temp =
        case Temp.Map.find (initial, temp) of
          SOME _ => true
        | NONE => false
      (* Removes an unconnected node from the graph *)
      fun removeIsolated (graph, id) =
        if G.outDegree' (graph, id) = 0
        then G.removeNode (graph, id)
        else graph
      (* Attempts to move `temp` from `frozenSet` to `simplifySet`.
      `temp` must be in `fronzenSet`. *)
      fun frozenToSimplify (temp, igraph, activeMoves, pendingMoves, simplifySet, frozenSet) =
        if not (precolored temp)
        andalso not (moveRelated (temp, activeMoves, pendingMoves))
        andalso G.outDegree' (igraph, temp) < nReg
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
              val degree = G.outDegree' (igraph, t)
              val degree =
                if G.isAdjacent (u, t) andalso G.isAdjacent (v, t)
                then degree - 1
                else degree
            in if degree < nReg then cnt else cnt + 1
            end
        in (Temp.Set.foldl f 0 neighbors) < nReg
        end
      fun combineNodes (graph, u, v) =
        case G.getNode' (graph, v) of
          NONE => graph
        | SOME node =>
          let
            val graph = G.addNewNode (graph, u, ())
            val graph =
              G.foldSuccs (fn (t, g) => G.doubleEdge (g, u, t))
              graph node
            val graph = G.removeNode (graph, v)
          (* For our use cases here, this final `removeIsolated` is probably not
          necessary. But just keeping it here for safety. *)
          in removeIsolated (graph, u)
          end

      (* Unfreeze helpers *)
      fun unfreezeNeighbors (temp, igraph, pendingMoves, simplifySet, frozenSet) =
        let
          fun foldAdj (t, (pendingMoves, simplifySet, frozenSet)) =
            (* We are only dealing with `pendingMoves` here because
            `activeMoves` should be empty *)
            if G.outDegree' (pendingMoves, t) = 0
            then
              (* Remove `t` from `pendingMoves` if it is not move-related anymore *)
              let val pendingMoves = G.removeNode (pendingMoves, t)
              in
                (* If `t` is in `frozenSet` move it to `simplifySet` *)
                if G.outDegree' (igraph, t) < nReg
                then (
                  pendingMoves,
                  Temp.Set.add (simplifySet, t),
                  Temp.Set.delete (frozenSet, t)
                )
                else (pendingMoves, simplifySet, frozenSet)
              end
            else (pendingMoves, simplifySet, frozenSet)
        val adj = G.succs (G.getNode (pendingMoves, temp))
        val pendingMoves = G.removeNode (pendingMoves, temp)
        in foldl foldAdj (pendingMoves, simplifySet, frozenSet) adj
        end

      val colors = foldl Set.add' Set.empty registers
      fun assignColor (node, igraph, colorMap) =
        let
          fun foldSucc (t, avail) =
            Set.delete (avail, valOf (Temp.Map.find (colorMap, t)))
            handle LibBase.NotFound => avail
          val availColor = G.foldSuccs foldSucc colors node
        in
          case Set.find (fn _ => true) availColor of
            SOME color => (
              true,
              Temp.Map.insert (
                colorMap,
                G.getNodeID node,
                color
              )
            )
          | NONE => (false, colorMap)
        end
      fun assignColor' (node, igraph, colorMap) =
        let val (success, colorMap) = assignColor (node, igraph, colorMap)
        in if success then colorMap else ErrorMsg.impossible "no available color"
        end

      fun simplify (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        case getItem simplifySet of
          NONE => coalesce (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        | SOME temp =>
          let
            val simplifySet = Temp.Set.delete (simplifySet, temp)
            (* We do not need to adjust `activeMoves` and `pendingMoves` here because `temp` must not
            be in either of them *)
          in removeTemp (temp, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          end
      and coalesce (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let val activeMoves' = G.nodes activeMoves
        in
          if List.null activeMoves'
          then unfreeze (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          else
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
                in coalesce (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
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
                    in coalesce (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
                    end
                end
            end
        end
      and combine (u, v, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val node = G.getNode (igraph, v)
          val igraph' = combineNodes (igraph, u, v)
          val activeMoves = combineNodes (activeMoves, u, v)
          val pendingMoves = combineNodes (pendingMoves, u, v)
          (* `v` should not be in `simplifySet` *)
          val frozenSet = Temp.setDelete (frozenSet, v)
          val spillSet = Temp.setDelete (spillSet, v)
          val (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
            adjustWorklist (node, igraph', activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          val (frozenSet, spillSet) =
            if G.outDegree' (igraph', u) < nReg
            then (frozenSet, spillSet)
            else (Temp.setDelete (frozenSet, u), Temp.Set.add (spillSet, u))
          (* Since we just removed a move related to `u`, try to move it to `simplifySet` *)
          val (fronzenSet, simplifySet) = frozenToSimplify (
            u, igraph', activeMoves, pendingMoves, simplifySet, frozenSet
          )
          val (colorMap, spillMap) =
            simplify (igraph', activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          val (success, colorMap) = assignColor (node, igraph, colorMap)
        in
          if success then (colorMap, spillMap)
          else (
            colorMap,
            Temp.Map.insert (
              spillMap,
              v,
              valOf (Temp.Map.find (spillMap, u))
            )
          )
        end
      and unfreeze (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        case getItem frozenSet of
          NONE => spill (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
        | SOME temp => 
          let
            val simplifySet = Temp.Set.add (simplifySet, temp)
            val frozenSet = Temp.Set.delete (frozenSet, temp)
            val (pendingMoves, simplifySet, frozenSet) =
              unfreezeNeighbors (temp, igraph, pendingMoves, simplifySet, frozenSet)
          in simplify (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          end
      and spill (igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        if Temp.Set.equal (spillSet, Temp.Set.empty)
        then (Temp.Map.empty, Temp.Map.empty)
        else
          let
            fun selectSpill (temp, (curSpill, curMin)) =
              let val cost = spillCost temp / Real.fromInt (G.outDegree' (igraph, temp))
              in
                if cost < curMin
                then (temp, cost)
                else (curSpill, curMin)
              end
            val (spill', cost) = Temp.Set.foldl selectSpill (F.ZERO, Real.posInf) spillSet
          in
            if spill' = F.ZERO
            then ErrorMsg.impossible "have to re-spill"
            else
              let
                val spillSet = Temp.Set.delete (spillSet, spill')
                val (pendingMoves, simplifySet, frozenSet) =
                  unfreezeNeighbors (spill', igraph, pendingMoves, simplifySet, frozenSet)
              in removeTemp (spill', igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
              end
          end
      and removeTemp (temp, igraph, activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
        let
          val node = G.getNode (igraph, temp)
          val igraph' = G.removeNode (igraph, temp)
          val (activeMoves, pendingMoves, simplifySet, frozenSet, spillSet) =
            adjustWorklist (node, igraph', activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          val (colorMap, spillMap) =
            simplify (igraph', activeMoves, pendingMoves, simplifySet, frozenSet, spillSet)
          val (success, colorMap) = assignColor (node, igraph, colorMap)
        in
          if success then (colorMap, spillMap)
          else (colorMap, Temp.Map.insert (spillMap, temp, temp))
        end
    in
      (Frame.tempMap, [])
    end

end