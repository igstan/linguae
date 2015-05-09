package ro.igstan.debugger
package syntax

sealed trait Term
object Term {
  case class Num(value: Int) extends Term
  case class Var(name: String) extends Term
  case class Fun(param: String, body: Term) extends Term
  case class App(fn: Term, arg: Term) extends Term
}
