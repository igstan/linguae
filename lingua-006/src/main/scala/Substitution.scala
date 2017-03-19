package codecamp

case class Substitution(solutions: Map[Type.Var, Type]) {
  def apply(constraints: Set[Constraint]): Set[Constraint] = {
    constraints.map(c => apply(c))
  }

  def apply(constraint: Constraint): Constraint = {
    Constraint(
      apply(constraint.a),
      apply(constraint.b)
    )
  }

  def apply(ty: Type): Type = {
    solutions.foldLeft(ty) { (result, solution) =>
      val (tvar, solutionType) = solution
      substitute(result, tvar, solutionType)
    }
  }

  def substitute(ty: Type, tvar: Type.Var, replacement: Type): Type = {
    ty match {
      case Type.INT => ty
      case Type.BOOL => ty
      case Type.FUN(paramTy, returnTy) =>
        Type.FUN(
          substitute(paramTy, tvar, replacement),
          substitute(returnTy, tvar, replacement)
        )
      case Type.VAR(tvar2) =>
        if (tvar == tvar2) replacement else ty
    }
  }

  def compose(other: Substitution): Substitution = {
    val substitutedThis = solutions.mapValues(s => other.apply(s))
    Substitution(substitutedThis ++ other.solutions)
  }
}

object Substitution {
  def empty = Substitution(Map.empty)

  def fromPair(tvar: Type.Var, ty: Type): Substitution = {
    Substitution(Map(tvar -> ty))
  }
}
