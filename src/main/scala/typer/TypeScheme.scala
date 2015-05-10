package ro.igstan.debugger
package typer

case class TypeScheme(vars: Set[Type.Var], ty: Type)

object TypeScheme {
  case class Var(value: Int) extends AnyRef

  object Var {
    private var counter = -1

    def fresh(): Var = {
      counter += 1
      Var(counter)
    }

    def reset(): Unit = {
      counter = -1
    }
  }

  def forall(vars: Set[Type.Var], ty: Type): TypeScheme = {
    TypeScheme(vars: Set[Type.Var], ty: Type)
  }
}
