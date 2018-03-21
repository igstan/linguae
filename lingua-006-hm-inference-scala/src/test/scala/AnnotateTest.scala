package codecamp
package test

class AnnotateTest extends Test {
  val tenv = TypeEnv(Map(
    "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
    "-" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT))
  ))

  test("annotate identity") {
    val term = FUN("a", VAR("a"))
    val typedTerm = Annotate.annotate(term, tenv)

    inside (typedTerm) {
      case TypedTerm.FUN(
        t1,
        TypedTerm.Binder(t2, "a"),
        TypedTerm.VAR(t3, "a")
      ) =>
        t1 should not be(t2)
        t1 should not be(t3)
        t2 should be(t3)
    }
  }

  test("annotate const") {
    val term = FUN("a", FUN("b", VAR("a")))
    val typedTerm = Annotate.annotate(term, tenv)

    inside (typedTerm) {
      case TypedTerm.FUN(
        t1,
        TypedTerm.Binder(t2, "a"),
        TypedTerm.FUN(
          t3,
          TypedTerm.Binder(t4, "b"),
          TypedTerm.VAR(t5, "a")
        )
      ) =>
        Set(t2, t3, t4, t5) should not contain(t1)
        Set(t1, t3, t4) should not contain(t2)
        Set(t1, t2, t4, t5) should not contain(t3)
        Set(t1, t2, t3, t5) should not contain(t4)
        Set(t1, t3, t4) should not contain(t5)
        t2 should be(t5)
    }
  }

  test("annotate compose") {
    val term = FUN("f", FUN("g", FUN("x", APP(VAR("f"), APP(VAR("g"), VAR("x"))))))
    val typedTerm = Annotate.annotate(term, tenv)

    inside (typedTerm) {
      case TypedTerm.FUN(
        t1,
        TypedTerm.Binder(t2, "f"),
        TypedTerm.FUN(
          t3,
          TypedTerm.Binder(t4, "g"),
          TypedTerm.FUN(
            t5,
            TypedTerm.Binder(t6, "x"),
            TypedTerm.APP(
              t7,
              TypedTerm.VAR(t8, "f"),
              TypedTerm.APP(
                t9,
                TypedTerm.VAR(t10, "g"),
                TypedTerm.VAR(t11, "x")
              )
            )
          )
        )
      ) =>
        Set(t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) should not contain(t1)
        Set(t1, t3, t4, t5, t6, t7, t9, t10, t11) should not contain(t2)
        Set(t1, t2, t4, t5, t6, t7, t8, t9, t10, t11) should not contain(t3)
        Set(t1, t2, t3, t5, t6, t7, t8, t9, t11) should not contain(t4)
        Set(t1, t2, t3, t4, t6, t7, t8, t9, t10, t11) should not contain(t5)
        Set(t1, t2, t3, t4, t5, t7, t8, t9, t10) should not contain(t6)
        Set(t1, t2, t3, t4, t5, t6, t8, t9, t10, t11) should not contain(t7)
        Set(t1, t3, t4, t5, t6, t7, t9, t10, t11) should not contain(t8)
        Set(t1, t2, t3, t4, t5, t6, t7, t8, t10, t11) should not contain(t9)
        Set(t1, t2, t3, t5, t6, t7, t8, t9, t11) should not contain(t10)
        Set(t1, t2, t3, t4, t5, t7, t8, t9, t10) should not contain(t11)
        t2 should be(t8)
        t4 should be(t10)
        t6 should be(t11)
    }
  }

  test("annotate pred") {
    val term = FUN(
      "pred",
      IF(
        APP(VAR("pred"), INT(1)),
        INT(2),
        INT(3)
      )
    )
    val typedTerm = Annotate.annotate(term, tenv)

    inside (typedTerm) {
      case TypedTerm.FUN(
        t1,
        TypedTerm.Binder(t2, "pred"),
        TypedTerm.IF(
          t3,
          TypedTerm.APP(
            t4,
            TypedTerm.VAR(t5, "pred"),
            TypedTerm.INT(t6, 1)
          ),
          TypedTerm.INT(t7, 2),
          TypedTerm.INT(t8, 3)
        )
      ) =>
        Set(t2, t3, t4, t5, t6, t7, t8) should not contain(t1)
        Set(t1, t3, t4, t6, t7, t8) should not contain(t2)
        Set(t1, t2, t4, t5, t6, t7, t8) should not contain(t3)
        Set(t1, t2, t3, t5, t6, t7, t8) should not contain(t4)
        Set(t1, t3, t4, t6, t7, t8) should not contain(t5)
        Set(t1, t2, t3, t4, t5, t7, t8) should not contain(t6)
        Set(t1, t2, t3, t4, t5, t6, t8) should not contain(t7)
        Set(t1, t2, t3, t4, t5, t6, t7) should not contain(t8)
        t2 should be(t5)
    }
  }
}
