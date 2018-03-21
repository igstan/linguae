structure LivenessAnalysis :> LIVENESS_ANALYSIS =
struct
  structure NodeMap = Graph.NodeMap
  structure TempSet = Graph.TempSet

  datatype igraph =
    IGRAPH of {
      graph : Graph.graph,
      nodeMap : Temp.temp Graph.node_map,
      moves : (Graph.node * Graph.node) list
    }

  fun interferenceGraph (Flow.FGRAPH { control, def, use, isMove }) =
    let
      fun equalMaps a b =
        NodeMap.alli (fn (node, set1) =>
          case NodeMap.find (b, node) of
            NONE => raise Case.Unreachable
          | SOME set2 => TempSet.equal (set1, set2)
        ) a

      (*
       * Dataflow equations for liveness analysis:
       *
       * 1:  in[n] = use[n] ∪ (out[n] - def[n])
       * 2: out[n] = ∪(in[s]) where s ∈ succ[n]
       *
       * Note that "use" and "def" sets may also be called "gen" and "kill"
       * sets in the literature.
       *)
      fun solveEquations inSets outSets =
        let
          fun loop prevIn prevOut currIn currOut =
            let
              val fixpointReached =
                equalMaps prevIn currIn andalso equalMaps prevOut currOut
            in
              if fixpointReached
              then (currIn, currOut)
              else
                let
                  (* 1: in[n] = use[n] ∪ (out[n] - def[n]) *)
                  val newIn = NodeMap.mapi (fn (node, _) =>
                    let
                      val outTempSet =
                        case NodeMap.find (currOut, node) of
                          NONE => raise Fail ("bug: no <out> entry for node: " ^ Graph.nodename node)
                        | SOME temps => temps
                      val defTempSet =
                        case NodeMap.find (def, node) of
                          NONE => raise Fail ("bug: no <def> entry for node: " ^ Graph.nodename node)
                        | SOME temps => temps
                      val useTempSet =
                        case NodeMap.find (use, node) of
                          NONE => raise Fail ("bug: no <use> entry for node: " ^ Graph.nodename node)
                        | SOME temps => temps
                    in
                      TempSet.union (useTempSet, TempSet.difference (outTempSet, defTempSet))
                    end
                  ) currIn

                  (* 2: out[n] = ∪(in[s]) where s ∈ succ[n] *)
                  val newOut = NodeMap.mapi (fn (node, _) =>
                    let
                      val succ = Graph.succ node
                      val inTempSets = List.map (fn s =>
                        case NodeMap.find (currIn, s) of
                          NONE => raise Fail ("bug: no <in> entry for node: " ^ Graph.nodename node)
                        | SOME n => n
                      ) succ
                    in
                      List.foldl TempSet.union TempSet.empty inTempSets
                    end
                  ) currOut
                in
                  loop currIn currOut newIn newOut
                end
            end
        in
          loop inSets outSets inSets outSets
        end

      val sortedNodes = TopoSort.sort control

      val (inSets, outSets) =
        let
          fun fold (node, map) = NodeMap.insert (map, node, TempSet.empty)
          val emptySets = List.foldl fold NodeMap.empty sortedNodes
        in
          (emptySets, emptySets)
        end

      val (_, liveoutSets) = solveEquations inSets outSets

      val igraph = buildIgraph def isMove liveoutSets
    in
      (igraph, liveoutSets)
    end

  and buildIgraph def isMove liveoutSets =
    (*
     * For each node in liveoutSets, look up the definitions. For each
     * definition, add an edge between that definition and each of the
     * temporaries associated with that node in the liveoutSets.
     *)
    let
      val igraph = Graph.newGraph ()

      fun linkTempToDef defNode isMove def (temp, nodeMap) =
        (* Don't add interference edges for copy instructions. *)
        if isMove andalso def = temp
        then nodeMap
        else
          let
            val tempNode = Graph.newNode igraph
          in
            Graph.mkEdge { from = defNode, to = tempNode }
          ; NodeMap.insert (nodeMap, tempNode, temp)
          end

      fun linkDefToTemps tempSet isMove (def, nodeMap) =
        let
          val defNode = Graph.newNode igraph
          val nodeMap = NodeMap.insert (nodeMap, defNode, def)
        in
          TempSet.foldl (linkTempToDef defNode isMove def) nodeMap tempSet
        end

      fun linkDefs (node, tempSet, nodeMap) =
        case NodeMap.find (def, node) of
          NONE => NodeMap.empty
        | SOME defSet =>
          let
            val isMove = Option.getOpt (NodeMap.find (isMove, node), false)
          in
            TempSet.foldl (linkDefToTemps tempSet false) nodeMap defSet
          end

      val nodeMap = NodeMap.empty
      val nodeMap = NodeMap.foldli linkDefs nodeMap liveoutSets
    in
      IGRAPH {
        graph = igraph,
        nodeMap = nodeMap,
        moves = []
      }
    end

  fun show (outstream, igraph) = raise Fail "not implemented"
end
