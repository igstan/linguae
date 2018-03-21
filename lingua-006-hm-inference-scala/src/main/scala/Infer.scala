package codecamp

object Infer {
  val tenv = TypeEnv(Map(
    "+" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT)),
    "-" -> Type.FUN(Type.INT, Type.FUN(Type.INT, Type.INT))
  ))

  def typeOf(term: Term): Type = {
    val typedTerm = Annotate.annotate(term, tenv)
    val constraints = Constraint.collect(typedTerm)
    val subst = Unifier.unify(constraints)
    subst.apply(typedTerm.ty)
  }
}
