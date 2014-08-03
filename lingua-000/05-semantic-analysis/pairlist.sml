structure PairList : PAIR_LIST =
struct
  fun zipOption (xs, ys) =
    let
      fun loop lists result =
        case lists of
          ([], []) => result
        | (x :: xs, []) => loop (xs, []) ((SOME(x), NONE) :: result)
        | ([], y :: ys) => loop ([], ys) ((NONE, SOME(y)) :: result)
        | (x :: xs, y :: ys) => loop (xs, ys) ((SOME(x), SOME(y)) :: result)
    in
      List.rev (loop (xs, ys) [])
    end
end
