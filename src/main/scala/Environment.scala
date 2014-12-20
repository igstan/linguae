package toy

case class Environment(bindings: Map[String, Value]) {
  def get(name: String): Option[Value] = {
    bindings.get(name)
  }

  def set(name: String, value: Value): Environment = {
    Environment(bindings + (name -> value))
  }

  def merge(other: Environment): Environment = {
    Environment(bindings ++ other.bindings)
  }
}

object Environment {
  def empty = Environment(Map.empty)
}
