package toy

case class Environment(bindings: Map[String, Value]) {
  def get(name: String): Option[Value] = {
    bindings.get(name)
  }

  def set(name: String, value: Value): Environment = {
    Environment(bindings + (name -> value))
  }
}

object Environment {
  def empty = Environment(Map.empty)
}
