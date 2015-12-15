package ro.igstan.debugger
package typer

case class Substitution(solutions: Map[Type.Var, Type]) {
  def apply(ty: Type): Type = {
    ???
  }

  def compose(other: Substitution): Substitution = {
    ???
  }
}

object Substitution {
  def empty: Substitution = {
    Substitution(Map.empty)
  }

  def one(tvar: Type.Var, ty: Type): Substitution = {
    Substitution(Map(tvar -> ty))
  }
}
