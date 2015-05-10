package ro.igstan.debugger
package typer

case class TypeEnv(bindings: Map[String, TypeScheme]) {
  def get(name: String): Option[TypeScheme] = {
    bindings.get(name)
  }

  def set(name: String, value: TypeScheme): TypeEnv = {
    TypeEnv(bindings + (name -> value))
  }

  def generalize(ty: Type): TypeScheme = {
    ???
  }
}

object TypeEnv {
  def empty = TypeEnv(Map.empty)
}

