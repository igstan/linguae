package ro.igstan.debugger
package eval

case class Env(bindings: Map[String, Value]) {
  def get(name: String): Option[Value] = bindings.get(name)
  def set(name: String, value: Value): Env = Env(bindings + (name -> value))
}

object Env {
  import Value._

  def empty = Env(Map.empty)

  def predef = Env(Map(
    "+" -> Native(plus),
    "-" -> Native(minus),
    "*" -> Native(times),
    "/" -> Native(div)
  ))

  private def plus(a: Value): Either[String, Value] = arith(a, _ + _)
  private def minus(a: Value): Either[String, Value] = arith(a, _ - _)
  private def times(a: Value): Either[String, Value] = arith(a, _ * _)
  private def div(a: Value): Either[String, Value] = arith(a, _ / _)

  private def arith(a: Value, op: (Int,Int) => Int) = {
    a match {
      case Num(a) => Right(Native {
        case Num(b) => Right(Num(op(a, b)))
        case Fun(param, body, closure) => Left("second operand was a function")
        case Native(fn) =>  Left("second operand was a function")
      })
      case Fun(param, body, closure) => Left("first operand was a function")
      case Native(fn) => Left("first operand was a function")
    }
  }
}
