package linguae

import linguae.Expr.{ BinOp, Const }

sealed trait Expr extends Product with Serializable {
  def pretty(operatorTable: Map[String, Fixity]): String = {
    def parenthesize(a: Expr, parentFixity: Fixity) =
      a match {
        case Const(a) => a.toString
        case a @ BinOp(op, _, _) =>
          val thisFixity = operatorTable(op)
          val sa = a.pretty(operatorTable)
          if (parentFixity.precedence > thisFixity.precedence) s"($sa)" else sa
      }

    this match {
      case Const(n) => n.toString
      case BinOp(op, a, b) =>
        val oPrec = operatorTable(op)
        val sa = parenthesize(a, oPrec)
        val sb = parenthesize(b, oPrec)
        s"$sa $op $sb"
    }
  }
}

object Expr {
  final case class Const(value: Int) extends Expr
  final case class BinOp(value: String, a: Expr, b: Expr) extends Expr
}
