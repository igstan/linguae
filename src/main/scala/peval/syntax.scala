package peval

import cats.implicits._

final case class Program(defs: Program.Defs, main: Expr) {
  override def toString: String = {
    (defs ++ Map("main" -> Def(List.empty, main)))
      .map {
        case (name, body) => s"[def $name $body]"
      }
      .mkString("\n")
  }
}

object Program {
  type Defs = Map[Def.Name, Def]
}

final case class Def(params: List[String], body: Expr) {
  override def toString: String =
    s"[fun [${params.mkString(" ")}] $body]"
}

object Def {
  type Name = String
}

sealed trait Val extends Any with Product with Serializable {
  override def toString: String =
    this match {
      case Val.I(value) => s"$value"
      case Val.B(value) => s"$value"
    }
}

sealed trait Expr extends Product with Serializable {
  /** Determine whether the expression tree contains `Apply` nodes. */
  def hasApplies: Boolean =
    this match {
      case Expr.Const(_) => false
      case Expr.Var(_) => false
      case Expr.Apply(_, _) => true
      case Expr.Prim(_, a, b) => a.hasApplies || b.hasApplies
      case Expr.If(a, b, c) => a.hasApplies || b.hasApplies || c.hasApplies
    }

  override def toString: String =
    this match {
      case Expr.Const(value) => value.toString
      case Expr.Var(name) => name
      case Expr.Apply(fn, args) => s"[$fn ${args.mkString(" ")}]"
      case Expr.Prim(op, a, b) => s"[$op $a $b]"
      case Expr.If(a, b, c) => s"[if $a $b $c]"
    }
}

sealed trait Op extends Product with Serializable {
  override def toString: String =
    this match {
      case Op.Eqv => "="
      case Op.Add => "+"
      case Op.Sub => "-"
      case Op.Mul => "*"
    }

  def apply(a: Val, b: Val): Val =
    (a, b) match {
      case (Val.I(va), Val.I(vb)) =>
        this match {
          case Op.Eqv => Val.B(va === vb)
          case Op.Add => Val.I(va + vb)
          case Op.Sub => Val.I(va - vb)
          case Op.Mul => Val.I(va * vb)
        }

      case (Val.B(va), Val.B(vb)) =>
        this match {
          case Op.Eqv => Val.B(va === vb)
          case _ => sys.error("boolean values can only be compared for equality")
        }

      case _ => sys.error("operands of different types")
    }
}

object Val {
  final case class I(value: Int) extends AnyVal with Val
  final case class B(value: Boolean) extends AnyVal with Val
}

object Expr {
  final case class Const(value: Val) extends Expr
  final case class Var(name: String) extends Expr
  final case class Apply(fn: String, args: List[Expr]) extends Expr
  final case class Prim(op: Op, a: Expr, b: Expr) extends Expr
  final case class If(a: Expr, b: Expr, c: Expr) extends Expr
}

object Op {
  case object Eqv extends Op
  case object Add extends Op
  case object Sub extends Op
  case object Mul extends Op
}
