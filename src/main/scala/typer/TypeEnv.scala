package ro.igstan.debugger
package typer

case class TypeEnv(bindings: Map[String, Type]) {
  def get(name: String): Option[Type] = bindings.get(name)
  def set(name: String, value: Type): TypeEnv = TypeEnv(bindings + (name -> value))

  def generalize(ty: Type): TypeScheme = ???
}

object TypeEnv {
  def empty = TypeEnv(Map.empty)
}

