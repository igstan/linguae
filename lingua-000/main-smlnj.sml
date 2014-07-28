structure Main =
struct
  fun main (name, args) =
    let
      val _ = Tree.main ()
    in
      0
    end
end
