package ro.igstan.debugger
package syntax

sealed trait Term {
  val meta: String
  val id: String
}

object Term {
  case class INT(value: Int)(val meta: String, val id: String) extends Term
  case class ADD(a: Term, b: Term)(val meta: String, val id: String) extends Term
  case class SUB(a: Term, b: Term)(val meta: String, val id: String) extends Term
  case class BOOL(value: Boolean)(val meta: String, val id: String) extends Term
  case class VAR(name: String)(val meta: String, val id: String) extends Term
  case class IF(test: Term, yes: Term, no: Term)(val meta: String, val id: String) extends Term
  case class FN(param: String, body: Term)(val meta: String, val id: String) extends Term
  case class APP(fn: Term, arg: Term)(val meta: String, val id: String) extends Term
  case class LET(binding: String, value: Term, body: Term)(val meta: String, val id: String) extends Term
}
