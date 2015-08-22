structure Ref :> REF =
struct
  fun getAndIncrement cell =
    !cell before (cell := !cell + 1)
end
