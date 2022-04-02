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

    (*
     * Taken from: "Wand's Algorithm Extended For the Polymorphic ML-Let".
     *
     * ```sml
     * let
     *   val x1 = fn y => fn z => (z y) y
     * in
     *   let
     *     val x2 = fn z => x1 (x1 z)
     *   in
     *     let
     *       val x3 = fn z => x2 (x2 z)
     *     in
     *       let
     *         val x4 = fn z => x3 (x3 z)
     *       in
     *         x4 (fn z => z)
     *       end
     *     end
     *   end
     * end
     * ```
     *)
    val nestedLets =
      LET (
        "x1", FUN ("y", FUN ("z", APP (APP (VAR "z", VAR "y"), VAR "y"))),
        LET (
          "x2", FUN ("z", APP (VAR "x1", APP (VAR "x1", VAR "z"))),
          LET (
            "x3", FUN ("z", APP (VAR "x2", APP (VAR "x2", VAR "z"))),
            LET (
              "x4", FUN ("z", APP (VAR "x3", APP (VAR "x3", VAR "z"))),
              APP (VAR "x4", FUN ("z", VAR "z"))
            )
          )
        )
      )
  end
end
