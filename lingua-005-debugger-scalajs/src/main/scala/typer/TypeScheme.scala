package ro.igstan.debugger
package typer

sealed trait TypeScheme

object TypeScheme {
  case class Var(value: Int) extends AnyRef

  object Var {
    private var counter = -1

    def fresh(): TypeScheme = {
      counter += 1
      TSVAR(Var(counter))
    }

    def reset(): Unit = {
      counter = -1
    }
  }

  case class FORALL(vars: Set[Type.Var], ty: Type) extends TypeScheme
  case class TSVAR(tsVar: Var) extends TypeScheme
}
