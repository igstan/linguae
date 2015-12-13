signature COLOR =
sig
  structure Frame : FRAME
  structure TempMap : ORD_MAP where type Key.ord_key = Temp.temp

  type allocation = Frame.register TempMap.map

  val color :
    {
      interference : LivenessAnalysis.igraph,
      initial : allocation,
      spillCost : Graph.node -> int,
      registers : Frame.register list
    } -> allocation * Temp.temp list
end
