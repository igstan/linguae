package toy

sealed trait Value
object Value {
  case class Num(value: Int) extends Value
  case class Fun(param: String, body: Node) extends Value
}
