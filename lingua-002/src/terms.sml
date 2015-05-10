structure Terms =
struct
  local
    open Term
  in
    val predef = TypeEnv.fromList [
      ("+",     TypeScheme.FORALL ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
      ("-",     TypeScheme.FORALL ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
      ("*",     TypeScheme.FORALL ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
      ("/",     TypeScheme.FORALL ([], Type.FUN (Type.INT, Type.FUN (Type.INT, Type.INT)))),
      ("zero?", TypeScheme.FORALL ([], Type.FUN (Type.INT, Type.BOOL)))
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
          APP (VAR "id", BOOL true),
          APP (VAR "id", INT 1),
          APP (VAR "id", INT 0)
        )
      )

    (*
     * let
     *   val const = fn y =>
     *     let
     *       val f = fn x => y
     *     in
     *       f
     *     end
     * in
     *   if const true 1
     *   then 2
     *   else 3
     * end
     *)
    val closureLetPolymorphism =
      LET (
        "const", FUN ("y", LET ("f", FUN ("x", VAR "y"), VAR "f")),
        IF (
          APP (APP (VAR "const", BOOL true), INT 1),
          INT 2,
          INT 3
        )
      )
  end
end
