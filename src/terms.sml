structure Terms =
struct
  val identity = Term.FUN ("a", Term.VAR "a")
  val constant = Term.FUN ("a", Term.FUN ("b", Term.VAR "a"))
  val compose = Term.FUN ("f", Term.FUN ("g", Term.FUN ("x", Term.APP (Term.VAR "f", Term.APP (Term.VAR "g", Term.VAR "x")))))
  val counter = Term.FUN ("counter", Term.INT 1)
end
