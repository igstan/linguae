structure Flow =
struct
  structure Graph = Graph

  datatype flowgraph =
    FGRAPH of {
      control : Graph.graph,
      def : Graph.temp_set Graph.node_map,
      use : Graph.temp_set Graph.node_map,
      isMove : bool Graph.node_map
    }
end
