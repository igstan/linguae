package codecamp

object Annotate {
  def annotate(term: Term, tenv: TypeEnv): TypedTerm = {
    term match {
      case INT(value) => TypedTerm.INT(Type.freshVar(), value)
      case BOOL(value) => TypedTerm.BOOL(Type.freshVar(), value)
      case FUN(param, body) =>
        val paramTy = Type.freshVar()
        val paramBinder = TypedTerm.Binder(paramTy, param)
        val extendedTenv = tenv.set(param, paramTy)
        TypedTerm.FUN(Type.freshVar(), paramBinder, annotate(body, extendedTenv))
      case VAR(name) =>
        tenv.get(name) match {
          case None => throw new RuntimeException(s"unbound identifier: $name")
          case Some(ty) => TypedTerm.VAR(ty, name)
        }
      case APP(fn, arg) =>
        TypedTerm.APP(Type.freshVar(), annotate(fn, tenv), annotate(arg, tenv))
      case IF(testCondition, trueBranch, falseBranch) =>
        TypedTerm.IF(
          Type.freshVar(),
          annotate(testCondition, tenv),
          annotate(trueBranch, tenv),
          annotate(falseBranch, tenv)
        )
      case LET(binding, value, body) =>
        val bindingTy = Type.freshVar()
        val extendedEnv = tenv.set(binding, bindingTy)
        TypedTerm.LET(
          Type.freshVar(),
          TypedTerm.Binder(bindingTy, binding),
          annotate(value, extendedEnv),
          annotate(body, extendedEnv)
        )
    }
  }
}
