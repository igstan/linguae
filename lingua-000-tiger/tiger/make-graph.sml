structure MakeGraph :> MAKE_GRAPH =
struct
  structure A = Assem
  structure NodeMap = Graph.NodeMap
  structure TempSet = Graph.TempSet

  fun unionKeyedSet table key list =
    let
      val set =
        case NodeMap.find (table, key) of
          NONE => list
        | SOME value => TempSet.union (value, list)
    in
      NodeMap.insert (table, key, set)
    end

  fun instrs2graph instrs =
    let
      fun fold (instr, result as (flowGraph, nodes, sourceNodesForLabel, targetNodeForLabel, prevNode)) =
        let
          val Flow.FGRAPH { control, def, use, isMove } = flowGraph
          val instrNode = Graph.newNode control
          val resultNodes = instrNode :: nodes
        in
          case instr of
            A.OPER { src, dst, jump, ... } =>
            let
              fun fold (label, sourceNodesForLabel) =
                case Symbol.get targetNodeForLabel label of
                  SOME labelNode =>
                  let in
                    Graph.mkEdge { from = instrNode, to = labelNode }
                  ; sourceNodesForLabel
                  end
                | NONE =>
                    case Symbol.get sourceNodesForLabel label of
                      NONE => Symbol.set sourceNodesForLabel label [instrNode]
                    | SOME nodes => Symbol.set sourceNodesForLabel label (instrNode :: nodes)
              val newSourceNodesForLabel =
                case jump of
                  SOME jump => List.foldl fold sourceNodesForLabel jump
                | NONE =>
                  let in
                    case prevNode of
                      NONE => ()
                    | SOME prev => Graph.mkEdge { from = prev, to = instrNode }
                  ; sourceNodesForLabel
                  end
              val newFlowGraph = Flow.FGRAPH {
                control = control,
                def = unionKeyedSet def instrNode (TempSet.fromList dst),
                use = unionKeyedSet use instrNode (TempSet.fromList src),
                isMove = NodeMap.insert (isMove, instrNode, false)
              }
            in
              (newFlowGraph, resultNodes, sourceNodesForLabel, targetNodeForLabel, SOME instrNode)
            end
          | A.LABEL { lab, ... } =>
            let
              val newTargetNodeForLabel =
                case Symbol.get sourceNodesForLabel lab of
                  NONE => Symbol.set targetNodeForLabel lab instrNode
                | SOME nodes =>
                  let in
                    List.map (fn node => Graph.mkEdge { from = node, to = instrNode }) nodes
                  ; targetNodeForLabel
                  end
              val newFlowGraph = Flow.FGRAPH {
                control = control,
                def = def,
                use = use,
                isMove = NodeMap.insert (isMove, instrNode, false)
              }
            in
              (newFlowGraph, resultNodes, sourceNodesForLabel, newTargetNodeForLabel, SOME instrNode)
            end
          | A.MOVE { src, dst, ... } =>
            let
              val newFlowGraph = Flow.FGRAPH {
                control = control,
                def = unionKeyedSet def instrNode (TempSet.singleton dst),
                use = unionKeyedSet use instrNode (TempSet.singleton src),
                isMove = NodeMap.insert (isMove, instrNode, true)
              }
            in
              (newFlowGraph, resultNodes, sourceNodesForLabel, targetNodeForLabel, SOME instrNode)
            end
        end

      val control = Graph.newGraph ()
      val sourceNodesForLabel = Symbol.empty : Graph.node list Symbol.table
      val targetNodeForLabel = Symbol.empty : Graph.node Symbol.table
      val flowGraph = Flow.FGRAPH {
        control = control,
        def = NodeMap.empty,
        use = NodeMap.empty,
        isMove = NodeMap.empty
      }
      val seed = (flowGraph, [], sourceNodesForLabel, targetNodeForLabel, NONE)
      val (flowGraph, nodes, _, _, _) = List.foldl fold seed instrs
    in
      (flowGraph, List.rev nodes)
    end
end
