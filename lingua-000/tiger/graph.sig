signature GRAPH =
sig
  type node
  type graph

  structure Table : TABLE where type key = node

  exception GraphEdge

  val nodes : graph -> node list
  val succ : node -> node list
  val pred : node -> node list
  val adj : node -> node list
  val eq : node * node -> bool
  val newGraph : unit -> graph
  val newNode : graph -> node
  val mkEdge : { from : node, to : node } -> unit
  val rmEdge : { from : node, to : node } -> unit
  val nodename : node -> string
end
