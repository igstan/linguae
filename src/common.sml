(* Follows: "ML pattern match compilation (March 5, 1996).pdf" *)

structure Common =
  struct
    type con = { name : string, arity : int, span : int }

    datatype pat =
    | PVar of string
    | PCon of con * pat list

    datatype 'a tree =
    | Null
    | Leaf of 'a
    | Node of 'a tree * 'a * 'a tree

    val Nullc = { name = "Null", arity = 0, span = 3 }
    val Leafc = { name = "Leaf", arity = 1, span = 3 }
    val Nodec = { name = "Node", arity = 3, span = 3 }

    type 'rhs match = (pat * 'rhs) list
  end
