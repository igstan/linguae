(**
 * Sorts a graph topologically, without ensuring that the graph is acyclic.
 *)
structure TopoSort =
struct
  open Fn infix 1 |>

  structure G = Graph
  structure M = Graph.NodeMap

  fun sort (graph : G.graph) : G.node list =
    let
      fun fold (node, (result, marked)) =
        case M.find (marked, node) of
          SOME _ => (result, marked)
        | NONE => dfs node marked result

      and dfs node marked result =
        let
          val nodes = G.succ node
          val marked = M.insert (marked, node, true)
          val (result, marked) = List.foldl fold (result, marked) nodes
        in
          (node :: result, marked)
        end

      val seed = ([], M.empty)
      val nodes = G.nodes graph
    in
      List.foldl fold seed nodes |> #1
    end

  and main () =
    let
      val graph = G.newGraph ()
      val [a,b,c,d,e,f,g] = List.tabulate (7, fn _ => Graph.newNode graph)
      val _ = List.app (fn (a, b) => G.mkEdge { from = a, to = b }) [
        (a, b),
        (a, c),
        (a, f),
        (d, g),
        (d, f),
        (d, e),
        (f, c),
        (g, e),
        (g, a),
        (d, c),
        (b, e)
      ]
    in
      Show.list G.nodename (sort graph)
    end
end
