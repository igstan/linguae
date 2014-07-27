structure Main =
struct
  fun main (name, args) =
    let
      val _ = Lingua.interp Lingua.prog
    in
      0
    end
end
