package codecamp

case class Constraint(a: Type, b: Type)

object Constraint {
  import TypedTerm._

  def collect(typedTerm: TypedTerm): Set[Constraint] = {
    typedTerm match {
      case INT(ty, value) => Set(Constraint(ty, Type.INT))
      case BOOL(ty, value) => Set(Constraint(ty, Type.BOOL))
      case FUN(ty, param, body) =>
        collect(body) ++ Set(
          Constraint(ty, Type.FUN(param.ty, body.ty))
        )
      case VAR(ty, name) => Set.empty
      case APP(ty, fn, arg) =>
        collect(fn) ++ collect(arg) ++ Set(
          Constraint(fn.ty, Type.FUN(arg.ty, ty))
        )
      case IF(ty, testCondition, trueBranch, falseBranch) =>
        collect(testCondition) ++ collect(trueBranch) ++ collect(falseBranch) ++ Set(
          Constraint(testCondition.ty, Type.BOOL),
          Constraint(trueBranch.ty, ty),
          Constraint(falseBranch.ty, ty)
        )
      case LET(ty, binding, value, body) =>
        collect(value) ++ collect(body) ++ Set(
          Constraint(ty, body.ty),
          Constraint(binding.ty, value.ty)
        )
    }
  }
}
