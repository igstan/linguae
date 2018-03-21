package codecamp
package test

import codecamp.parser.Parser

class ConstraintTest extends Test {
  val tenv = TypeEnv(Map(
    "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
    "-" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT))
  ))

  test("constrains INT") {
    val t1 = Type.freshVar()
    val typedTerm = TypedTerm.INT(t1, 1)
    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.INT)
    ))
  }

  test("constrains BOOL") {
    val t1 = Type.freshVar()
    val typedTerm = TypedTerm.BOOL(t1, true)
    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.BOOL)
    ))
  }

  test("constrains FUN") {
    val t1 = Type.freshVar()
    val t2 = Type.freshVar()
    val t3 = Type.freshVar()

    val param = TypedTerm.Binder(t2, "param")
    val body = TypedTerm.VAR(t3, "param")
    val typedTerm = TypedTerm.FUN(t1, param, body)

    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.FUN(t2, t3))
    ))
  }

  test("constrains VAR") {
    val t1 = Type.freshVar()
    val typedTerm = TypedTerm.VAR(t1, "name")
    Constraint.collect(typedTerm) should be(Set.empty)
  }

  test("constrains APP") {
    val t1 = Type.freshVar()
    val t2 = Type.freshVar()
    val t3 = Type.freshVar()
    val fn = TypedTerm.VAR(t2, "fn")
    val arg = TypedTerm.VAR(t3, "arg")
    val typedTerm = TypedTerm.APP(t1, fn, arg)
    Constraint.collect(typedTerm) should be(Set(
      Constraint(t2, Type.FUN(t3, t1))
    ))
  }

  test("constrains IF") {
    val t1 = Type.freshVar()
    val t2 = Type.freshVar()
    val t3 = Type.freshVar()
    val t4 = Type.freshVar()
    val testCondition = TypedTerm.VAR(t2, "test")
    val trueBranch = TypedTerm.VAR(t3, "true")
    val falseBranch = TypedTerm.VAR(t4, "false")
    val typedTerm = TypedTerm.IF(t1, testCondition, trueBranch, falseBranch)
    Constraint.collect(typedTerm) should be(Set(
      Constraint(t2, Type.BOOL),
      Constraint(t3, t1),
      Constraint(t4, t1)
    ))
  }

  test("constrains LET") {
    val t1 = Type.freshVar()
    val t2 = Type.freshVar()
    val t3 = Type.freshVar()
    val t4 = Type.freshVar()
    val name = TypedTerm.Binder(t2, "name")
    val value = TypedTerm.VAR(t3, "value")
    val body = TypedTerm.VAR(t4, "body")
    val typedTerm = TypedTerm.LET(t1, name, value, body)
    Constraint.collect(typedTerm) should be(Set(
      Constraint(t2, t3),
      Constraint(t1, t4)
    ))
  }

  test("constrains identity") {
    val typedTerm @ TypedTerm.FUN(
      t1,
      TypedTerm.Binder(t2, "a"),
      TypedTerm.VAR(t3, "a")
    ) = Annotate.annotate(Parser.parse("""
      fn a => a
    """), tenv)

    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.FUN(t2, t3))
    ))
  }

  test("constrains const") {
    val typedTerm @ TypedTerm.FUN(
      t1,
      TypedTerm.Binder(t2, "a"),
      TypedTerm.FUN(
        t3,
        TypedTerm.Binder(t4, "b"),
        TypedTerm.VAR(t5, "a")
      )
    ) = Annotate.annotate(Parser.parse("""
      fn a => fn b => a
    """), tenv)

    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.FUN(t2, t3)),
      Constraint(t3, Type.FUN(t4, t5))
    ))
  }

  test("constrains compose") {
    val typedTerm @ TypedTerm.FUN(
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
    ) = Annotate.annotate(Parser.parse("""
      fn f => fn g => fn x => f (g x)
    """), tenv)

    Constraint.collect(typedTerm) should be(Set(
      Constraint(t1, Type.FUN(t2, t3)),
      Constraint(t3, Type.FUN(t4, t5)),
      Constraint(t5, Type.FUN(t6, t7)),
      Constraint(t8, Type.FUN(t9, t7)),
      Constraint(t10, Type.FUN(t11, t9))
    ))
  }
}
