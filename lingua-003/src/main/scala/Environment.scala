package toy

case class Environment(bindings: Map[String, Store.Location]) {
  def get(name: String): Option[Store.Location] = {
    bindings.get(name)
  }

  def set(name: String, location: Store.Location): Environment = {
    Environment(bindings + (name -> location))
  }
}

object Environment {
  def empty = Environment(Map.empty)
}
