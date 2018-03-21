signature LIVENESS_ANALYSIS =
sig
  datatype igraph =
    IGRAPH of {
      graph : Graph.graph,
      nodeMap : Temp.temp Graph.node_map,
      moves : (Graph.node * Graph.node) list
    }

  val interferenceGraph : Flow.flowgraph -> igraph * (Graph.temp_set Graph.node_map)
  val show : TextIO.outstream * igraph -> unit
end
