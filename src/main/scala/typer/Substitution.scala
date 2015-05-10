package ro.igstan.debugger
package typer

case class Substitution(solutions: Map[Type.Var, Type]) {
  def apply(ty: Type): Type = {
    ???
  }
}
