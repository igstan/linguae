package ro.igstan.debugger
package typer

sealed trait Type

object Type {
  case class Var(value: Int) extends AnyVal

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
