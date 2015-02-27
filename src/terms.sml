structure Terms =
struct
  open Term

  val predefinedTypeEnv = TypeEnv.fromList [
    ("+",     TypeScheme.ForAll ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
    ("-",     TypeScheme.ForAll ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
    ("*",     TypeScheme.ForAll ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
    ("/",     TypeScheme.ForAll ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
    ("zero?", TypeScheme.ForAll ([], Type.FUN (Type.INT, Type.BOOL)))
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

  val letTerm = FUN ("x",
    LET (
      "y", FUN ("a", VAR "a"),
      APP (VAR "y", BOOL true)
    )
  )

  val letPolymorphism =
    LET ("id", FUN ("a", VAR "a"),
      IF (
        APP (FUN ("a", VAR "a"), BOOL true),
        APP (FUN ("a", VAR "a"), INT 1),
        APP (FUN ("a", VAR "a"), INT 0)
      )
    )
end
