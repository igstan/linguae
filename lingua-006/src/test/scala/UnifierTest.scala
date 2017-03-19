package codecamp
package test

class UnifierTest extends Test {
  test("unifies two ints") {
    val subst = Unifier.unify(Set(
      Constraint(Type.INT, Type.INT)
    ))
    subst should be(Substitution.empty)
  }

  test("unifies two bools") {
    val subst = Unifier.unify(Set(
      Constraint(Type.BOOL, Type.BOOL)
    ))
    subst should be(Substitution.empty)
  }

  test("unifies two functions") {
    val subst = Unifier.unify(Set(
      Constraint(
        Type.FUN(Type.BOOL, Type.BOOL),
        Type.FUN(Type.BOOL, Type.BOOL)
      )
    ))
    subst should be(Substitution.empty)
  }

  test("unifies two variables") {
    val subst = Unifier.unify(Set(
      Constraint(Type.VAR(1), Type.VAR(2))
    ))
    subst should be(Substitution(Map(
      1 -> Type.VAR(2)
    )))
  }

  test("unifies variable with non-variable type") {
    val subst = Unifier.unify(Set(
      Constraint(Type.VAR(1), Type.INT)
    ))
    subst should be(Substitution(Map(
      1 -> Type.INT
    )))
  }

  test("unifies variables in functions") {
    val subst = Unifier.unify(Set(
      Constraint(
        Type.FUN(Type.VAR(1), Type.BOOL),
        Type.FUN(Type.INT, Type.VAR(2))
      )
    ))
    subst should be(Substitution(Map(
      1 -> Type.INT,
      2 -> Type.BOOL
    )))
  }
}
