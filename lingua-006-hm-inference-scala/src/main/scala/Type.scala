package codecamp

sealed trait Type

object Type {
  type Var = Int

  case object INT extends Type
  case object BOOL extends Type
  case class FUN(paramTy: Type, returnTy: Type) extends Type
  case class VAR(tvar: Type.Var) extends Type

  private var counter = 0

  def freshVar(): Type = {
    counter += 1
    VAR(counter)
  }

  def resetFreshness(): Unit = {
    counter = 0
  }
}
