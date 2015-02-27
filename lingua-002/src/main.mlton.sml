structure Main =
struct
  fun main args =
    print (Infer.typeSignature Terms.compose Terms.predef ^ "\n")
end

val _ = Main.main (CommandLine.arguments ())
