package ro.igstan.debugger
package eval

case class Env(bindings: Map[String, Value]) {
  def get(name: String): Option[Value] = bindings.get(name)
  def set(name: String, value: Value): Env = Env(bindings + (name -> value))

  override def toString = {
    val pairs = bindings.map({
      case (k, Value.Num(n)) => s"$k: $n"
      case (k, Value.Bool(b)) => s"$k: $b"
      case (k, _: Value.Fun) => s"$k: <function>"
    })

    pairs.mkString("[", ",", "]")
  }
}

object Env {
  def empty = Env(Map.empty)
}
