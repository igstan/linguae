package codecamp
package test

class SubstitutionTest extends Test {
  test("substitutes standalone type variable") {
    val subst = Substitution.fromPair(1, Type.INT)
    subst.apply(Type.VAR(1)) should be(Type.INT)
  }

  test("substitutes type variable in functions") {
    val subst = Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL
    ))
    subst.apply(Type.FUN(Type.VAR(1), Type.VAR(2))) should be(Type.FUN(Type.INT, Type.BOOL))
  }

  test("substitutes constraint") {
    val subst = Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL
    ))
    subst.apply(Constraint(Type.VAR(1), Type.VAR(2))) should be(Constraint(Type.INT, Type.BOOL))
  }

  test("substitutes constraint set") {
    val subst = Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL,
      3 -> Type.FUN(Type.INT, Type.BOOL)
    ))
    val constraints = Set(
      Constraint(Type.VAR(1), Type.VAR(2)),
      Constraint(Type.VAR(2), Type.VAR(3))
    )
    subst.apply(constraints) should be(Set(
      Constraint(Type.INT, Type.BOOL),
      Constraint(Type.BOOL, Type.FUN(Type.INT, Type.BOOL))
    ))
  }

  test("composes") {
    val substA = Substitution(Map(
      3 -> Type.VAR(1),
      2 -> Type.INT,
      4 -> Type.FUN(Type.VAR(1), Type.VAR(2))
    ))
    val substB = Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL
    ))

    substA.compose(substB) should be(Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL, // variable in substB overrides that in substA
      3 -> Type.INT,
      4 -> Type.FUN(Type.INT, Type.BOOL)
    )))
  }
}
