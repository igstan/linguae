package ro.igstan.debugger
package eval

import syntax.Term

sealed trait Value

object Value {
  case class Num(value: Int) extends Value
  case class Bool(value: Boolean) extends Value
  case class Fun(param: String, body: Term, closure: Env) extends Value
  case class Native(fn: Value => Either[String, Value]) extends Value
}
