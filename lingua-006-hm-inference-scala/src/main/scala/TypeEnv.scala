package codecamp

case class TypeEnv(bindings: Map[String, Type]) {
  def set(name: String, ty: Type): TypeEnv = {
    TypeEnv(bindings + (name -> ty))
  }

  def get(name: String): Option[Type] = {
    bindings.get(name)
  }
}
