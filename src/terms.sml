structure Terms =
struct
  open Term

  val predefinedTEnv = TEnv.fromList [
    ("+", Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT))),
    ("-", Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT))),
    ("*", Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT))),
    ("/", Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT))),
    ("zero?", Type.FUN (Type.INT, Type.BOOL))
  ]

  val identity = FUN ("a", VAR "a")
  val constant = FUN ("a", FUN ("b", VAR "a"))
  val compose = FUN ("f", FUN ("g", FUN ("x", APP (VAR "f", APP (VAR "g", VAR "x")))))
  val counter = FUN ("counter", INT 1)
  val add10 = FUN ("x", APP (APP (VAR "+", VAR "x"), INT 10))

  val isZero = FUN ("x",
    IF (
      APP (VAR "zero?", VAR "x"),
      BOOL true,
      BOOL false
    )
  )
end
