structure Tree =
struct
  open Lang

  type key = string

  datatype 'a tree = LEAF
                   | TREE of 'a tree * key * 'a * 'a tree

  val empty = LEAF

  fun repeat s times =
    let
      fun go 0 acc = acc
        | go t acc = go (t - 1) acc ^ s
    in
      go times ""
    end

  fun showDepth LEAF depth = "LEAF"
    | showDepth (TREE(l, k, v, r)) depth =
      let
        val prefix = repeat " " 12
      in
        "[TREE left: " ^ (showDepth l (depth + 1)) ^ "\n" ^ (repeat prefix depth) ^
        "       key: " ^ "\"" ^ k ^ "\""           ^ "\n" ^ (repeat prefix depth) ^
        "     right: " ^ (showDepth r (depth + 1)) ^ "]"
      end

  (*
   * Sample Output
   *
   * [TREE left: LEAF
   *     key: "a"
   *   right: [TREE left: LEAF
   *                 key: "b"
   *               right: [TREE left: LEAF
   *                             key: "c"
   *                           right: LEAF]]]
   *)
  fun show tree = (showDepth tree 0) ^ "\n"

  fun insert LEAF key value = TREE(LEAF, key, value, LEAF)
    | insert (TREE(l, k, v, r)) key value =
      if key < k then TREE(insert l key value, k, v, r) else
      if key > k then TREE(l, k, v, insert r key value) else TREE(l, k, v, r)

  (* Exercise 1.1 a., page 12. *)
  fun member LEAF key = false
    | member (TREE(l, k, _, r)) key =
      if key < k then member l key else
      if key > k then member r key else true

  (* Exercise 1.1 b., page 12. *)
  fun lookup LEAF key = NONE
    | lookup (TREE(l, k, v, r)) key =
      if key < k then lookup l key else
      if key > k then lookup r key else SOME(v)

  fun main () =
    let
      val folder = fn (c, tree) => insert tree (Char.toString c) (Char.toString c)
      val tree1 = List.foldl folder LEAF (String.explode "tspipfbst")
      val tree2 = List.foldl folder LEAF (String.explode "abcdefghi")
    in
      (* Exercise 1.1 c., page 12. *)
      print <| (show tree1) ^ "\n" ^ (show tree2)
    end
end
