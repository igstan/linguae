package peval

final case class Program(defs: Program.Defs, main: Expr) {
  override def toString: String = {
    defs
      .map {
        case (name, body) => s"[def $name $body]"
      }
      .mkString(
        start = "",
        sep = "\n\n",
        end = s"\n[def main [] $main]",
      )
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
