structure ListPair : LIST_PAIR =
struct
  open ListPair

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

  fun zipWithIndex xs =
    let
      fun loop xs index result =
        case xs of
          [] => List.rev result
        | x :: xs => loop xs (index + 1) ((x, index) :: result)
    in
      loop xs 0 []
    end
end
