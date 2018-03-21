package codecamp

/**
 * Represents the untyped AST, produced by the parser phase.
 */
sealed trait Term

case class INT(value: Int) extends Term
case class BOOL(value: Boolean) extends Term
case class FUN(param: String, body: Term) extends Term
case class VAR(name: String) extends Term
case class APP(fn: Term, arg: Term) extends Term
case class IF(testCondition: Term, trueBranch: Term, falseBranch: Term) extends Term
case class LET(binding: String, value: Term, body: Term) extends Term
