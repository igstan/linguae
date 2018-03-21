package ro.igstan.debugger
package typer

sealed trait Type

object Type {
  case class Var(value: Int) extends AnyVal {
    def occursIn(ty: Type): Boolean = {
      ty match {
        case TINT => false
        case TBOOL => false
        case TVAR(other) => this == other
        case TFUN(paramTy, returnTy) => occursIn(paramTy) || occursIn(returnTy)
      }
    }
  }

  object Var {
    private var counter = -1

    def fresh(): Type = {
      counter += 1
      TVAR(Var(counter))
    }

    def reset(): Unit = {
      counter = -1
    }
  }
}

case class TFUN(paramTy: Type, returnTy: Type) extends Type
case class TVAR(tvar: Type.Var) extends Type
case object TBOOL extends Type
case object TINT extends Type
