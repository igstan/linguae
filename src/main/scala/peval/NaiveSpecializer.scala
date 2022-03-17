package peval

import cats.implicits._

object NaiveSpecializer {
  final case class Env(bindings: Map[String, Expr]) extends AnyVal

  object Env {
    val empty: Env = Env(Map.empty)
  }

  def specialize(program: Program): Expr = {
    def go(expr: Expr, env: Env): Expr =
      expr match {
        case Expr.Const(_) => expr

        case Expr.Var(v) =>
          env.bindings.get(v) match {
            case Some(e) => e
            case None => expr
          }

        case Expr.Prim(op, a, b) =>
          (go(a, env), go(b, env)) match {
            case (Expr.Const(va), Expr.Const(vb)) => Expr.Const(prim(op, va, vb))
            case (Expr.Const(Val.I(0)), a) if op == Op.Add => a
            case (a, Expr.Const(Val.I(0))) if op == Op.Add => a
            case (a, Expr.Const(Val.I(0))) if op == Op.Sub => a
            case (Expr.Const(Val.I(1)), a) if op == Op.Mul => a
            case (a, Expr.Const(Val.I(1))) if op == Op.Mul => a
            case (pa, pb) => Expr.Prim(op, pa, pb)
          }

        case Expr.If(cond, condT, condF) =>
          go(cond, env) match {
            case Expr.Const(Val.B(true)) => go(condT, env)
            case Expr.Const(Val.B(false)) => go(condF, env)
            case cond => Expr.If(cond, go(condT, env), go(condF, env))
          }

        case Expr.Apply(fn, args) =>
          program.defs.get(fn) match {
            case None => sys.error(show"undefined function: $fn")
            case Some(Def(params, body)) =>
              val pargs = args.map(go(_, env))
              go(body, Env(params.zip(pargs).toMap))
          }
      }

    go(program.main, Env.empty)
  }

  private def prim(op: Op, a: Val, b: Val): Val =
    (a, b) match {
      case (Val.I(va), Val.I(vb)) =>
        op match {
          case Op.Eqv => Val.B(va === vb)
          case Op.Add => Val.I(va + vb)
          case Op.Sub => Val.I(va - vb)
          case Op.Mul => Val.I(va * vb)
        }

      case (Val.B(va), Val.B(vb)) =>
        op match {
          case Op.Eqv => Val.B(va === vb)
          case _ => sys.error("boolean values can only be compared for equality")
        }

      case _ => sys.error("operands of different types")
    }
}
