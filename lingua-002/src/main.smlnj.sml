structure Main =
struct
  fun main () =
    print (Infer.typeSignature Terms.compose Terms.predef ^ "\n")
end
