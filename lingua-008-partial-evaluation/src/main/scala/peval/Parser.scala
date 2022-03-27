package peval

import scala.annotation.tailrec
import scala.util.Try
import peval.Program.Defs

object Parser {
  object INT {
    def unapply(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
  }

  def apply(sexpr: List[NODE]): Program = {
    @tailrec
    def loop(sexpr: List[NODE], main: Option[Expr], defs: Defs): Program =
      sexpr match {
        case Nil =>
          main match {
            case None => throw new RuntimeException("no main entry")
            case Some(main) => Program(defs, main)
          }

        case head :: tail =>
          head match {
            case LIST(ATOM("def"), ATOM("main"), LIST(), body) =>
              loop(tail, Some(apply(body)), defs)

            case LIST(ATOM("def"), ATOM(name), LIST(values @ _*), body) =>
              val args = values.toList.map {
                case ATOM(name) => name
                case _ => throw new RuntimeException(s"params must be atoms")
              }
              val func = Def(args, apply(body))

              loop(tail, main, defs + (name -> func))

            case expr =>
              throw new RuntimeException(s"illegal top-level expression: $expr")
          }
      }

    loop(sexpr, Option.empty, Map.empty)
  }

  def apply(sexpr: NODE): Expr =
    sexpr match {
      case LIST(ATOM("if"), a, b, c) => Expr.If(apply(a), apply(b), apply(c))
      case LIST(ATOM("="), a, b) => Expr.Prim(Op.Eqv, apply(a), apply(b))
      case LIST(ATOM("+"), a, b) => Expr.Prim(Op.Add, apply(a), apply(b))
      case LIST(ATOM("-"), a, b) => Expr.Prim(Op.Sub, apply(a), apply(b))
      case LIST(ATOM("*"), a, b) => Expr.Prim(Op.Mul, apply(a), apply(b))
      case LIST(ATOM(fun), args @ _*) => Expr.Apply(fun, args.map(apply).toList)
      case ATOM("true") => Expr.Const(Val.B(true))
      case ATOM("false") => Expr.Const(Val.B(true))

      case ATOM(name) =>
        try {
          Expr.Const(Val.I(name.toInt))
        } catch {
          case _: NumberFormatException => Expr.Var(name)
        }

      case _ => throw new RuntimeException(s"can't parse: $sexpr")
    }
}
