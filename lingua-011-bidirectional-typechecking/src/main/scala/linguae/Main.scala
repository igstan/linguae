package linguae

import scala.util.chaining.*

/**
 * STLC: Simply-Typed Lambda Calculus
 */
enum STLC {
  case Var(name: String)
  case App(fn: STLC, arg: STLC)
  case Abs(param: String, body: STLC)
  case If(cond: STLC, when: STLC, otherwise: STLC)
  case Ann(term: STLC, ty: Type)
  case True
  case False

  private val kw = Color.keyword

  override def toString: String =
    this match {
      case Var(name) => name
      case App(Var(fn), arg @ (Var(_) | True | False)) => s"$fn $arg"
      case App(Var(fn), arg) => s"$fn ($arg)"
      case App(fn, arg @ (Var(_) | True | False)) => s"($fn) $arg"
      case App(fn, arg) => s"($fn) ($arg)"
      case Abs(param, body) => s"${kw("fn")} $param => $body"
      case If(cond, when, otherwise) => s"${kw("when")} $cond ${kw("then")} $when ${kw("else")} $otherwise"
      case Ann(term, ty) => s"($term) : $ty"
      case True => kw("true")
      case False => kw("false")
    }
}

enum Type {
  case Bool
  case Fn(param: Type, ret: Type)

  override def toString: String =
    this match {
      case Bool            => "bool"
      case Fn(Fn(p, q), r) => s"($p → $q) → $r"
      case Fn(p, r)        => s"$p → $r"
    }
}

final class 𝝘(private val vals: Map[String, Type]) extends AnyVal {
  override def toString: String =
    vals.map({ case (k, t) => s"$k: $t" }).mkString("[", "; ", "]")

  def get(k: String): Option[Type] =
    vals.get(k)

  def put(k: String, t: Type): 𝝘 =
    𝝘(vals.updated(k, t))
}

object 𝝘 {
  val empty: 𝝘 = 𝝘(Map.empty)
}

object Bidi {
  private def logging[L, R](data: List[String], indent: Int)(fn: => Either[L, R]): Either[L, R] = {
    def red(s: String): String = "\u001b[31m%s\u001b[0m".format(s)
    def green(s: String): String = "\u001b[32m%s\u001b[0m".format(s)

    println(data.init.map(d => ("  " * indent) + d).mkString("\n"))

    fn.tap {
      case Left(v) => println(("  " * indent) + s"${red(data.last)} $v")
      case Right(v) => println(("  " * indent) + s"${green(data.last)} $v")
    }
  }

  def infer(term: STLC): Either[String, Type] =
    infer(𝝘.empty, term, indent = 0)

  def infer(context: 𝝘, term: STLC, indent: Int): Either[String, Type] = {
    val debug = List(
      s"INFER",
      s"|𝝘| $context",
      s"|𝘵| $term",
      s"INFER",
    )

    logging(debug, indent) {
      term match {
        //
        //  (x : 𝞽) ∈ 𝝘
        // ───────────── (BT-Var)
        //   𝝘 ⊢ x ⇒ 𝞽
        //
        case STLC.Var(x) =>
          context.get(x).toRight(s"undefined variable: $x")

        //
        // ─────────────── (BT-True)
        //  𝝘 ⊢ 𝘁𝗿𝘂𝗲 ⇒ 𝘉𝘰𝘰𝘭
        //
        case STLC.True => Right(Type.Bool)

        //
        // ─────────────── (BT-False)
        //  𝝘 ⊢ 𝗳𝗮𝗹𝘀𝗲 ⇒ 𝘉𝘰𝘰𝘭
        //
        case STLC.False => Right(Type.Bool)

        //
        //  𝝘 ⊢ 𝘵₁ : 𝘉𝘰𝘰𝘭   𝝘 ⊢ 𝘵₂ : 𝞽   𝝘 ⊢ 𝘵₃ : 𝞽
        // ─────────────────────────────────────── (BT-If)
        //          𝝘 ⊢ 𝗶𝗳 𝘵₁ 𝘁𝗵𝗲𝗻 𝘵₂ 𝗲𝗹𝘀𝗲 𝘵₃
        //
        case STLC.If(cond, when, otherwise) =>
          (
            infer(context, cond, indent + 1),
            infer(context, when, indent + 1),
            infer(context, otherwise, indent + 1),
          ) match {
            case (Right(t1), Right(t2), Right(t3)) =>
              if t2 == t3
              then Right(t2)
              else Left("mismatched types in `if` arms")

            case (left @ Left(_), _, _) => left
            case (_, left @ Left(_), _) => left
            case (_, _, left @ Left(_)) => left
          }

        //
        //    𝝘 ⊢ 𝘵 ⇐ 𝞽
        // ────────────── (BT-Ann)
        //  𝝘 ⊢ 𝘵 : 𝞽 ⇒ 𝞽
        //
        case STLC.Ann(term, ty) => check(context, term, ty, indent + 1)

        //
        //  𝝘 ⊢ 𝘵₁ ⇒ 𝞽₁ → 𝞽₂   𝝘 ⊢ 𝘵₂ ⇐ 𝞽₁
        // ────────────────────────────── (BT-App)
        //          𝝘 ⊢ 𝘵₁ 𝘵₂ ⇒ 𝞽₂
        //
        case STLC.App(fn, arg) =>
          infer(context, fn, indent + 1) match {
            case Right(Type.Fn(p, r)) =>
              check(context, arg, p, indent + 1).map(_ => r)
            case Right(t) =>
              Left(s"type mismatched: expected function type; got: $t")
            case left => left
          }

        case STLC.Abs(_, _) =>
          Left(s"type annotation required for lambda terms: $term")
      }
    }
  }

  def check(context: 𝝘, term: STLC, ty: Type, indent: Int): Either[String, Type] = {
    val debug = List(
      s"CHECK",
      s"|𝝘| $context",
      s"|𝘵| $term",
      s"|𝞽| $ty",
      s"CHECK",
    )

    logging(debug, indent) {
      term match {
        //
        //  𝝘 ⊢ 𝘵₁ ⇐ 𝘉𝘰𝘰𝘭   𝝘 ⊢ 𝘵₂ ⇐ 𝞽   𝝘 ⊢ 𝘵₃ ⇐ 𝞽
        // ─────────────────────────────────────── (BT-If)
        //        𝝘 ⊢ 𝗶𝗳 𝘵₁ 𝘁𝗵𝗲𝗻 𝘵₂ 𝗲𝗹𝘀𝗲 𝘵₃ ⇐ 𝞽
        //
        case STLC.If(cond, when, otherwise) =>
          (
            check(context, cond, Type.Bool, indent + 1),
            check(context, when, ty, indent + 1),
            check(context, otherwise, ty, indent + 1),
          ) match {
            case (Right(_), Right(_), Right(_)) => Right(ty)
            case (left @ Left(_), _, _)         => left
            case (_, left @ Left(_), _)         => left
            case (_, _, left @ Left(_))         => left
          }

        //
        //   𝝘, 𝘹 : 𝞽₁ ⊢ 𝘵 ⇐ 𝞽₂
        // ───────────────────── (BT-App)
        //  𝝘 ⊢ λ 𝘹. 𝘵 ⇐ 𝞽₁ → 𝞽₂
        //
        case STLC.Abs(param, body) =>
          ty match {
            case Type.Bool => Left(s"type mismatch: expected function; got: $ty")
            case fn @ Type.Fn(paramTy, ret) =>
              check(context.put(param, paramTy), body, ret, indent + 1).map(_ => fn)
          }

        // Small optimization? It wasn't present in the paper.
        case STLC.True | STLC.False =>
          if ty == Type.Bool
          then Right(Type.Bool)
          else Left(s"type mismatch: expected: boolean; got: $ty")

        //
        //  𝝘 ⊢ 𝘵 ⇒ 𝞽
        // ─────────── (BT-CheckInfer)
        //  𝝘 ⊢ 𝘵 ⇐ 𝞽
        //
        case _ =>
          infer(context, term, indent + 1) match {
            case Right(ty2) if ty == ty2 => Right(ty)
            case Right(ty2)              => Left(s"type mismatch: expected: $ty; got: $ty2")
            case left                    => left
          }
      }
    }
  }
}

object Main {
  import STLC.*
  import Type.*

  def main(args: Array[String]): Unit = {
    val tests =
      if args.isEmpty
      then Tests.values
      else args.map(Tests.valueOf)

    tests.foreach(_.run())
  }

  private enum Tests(test: => Unit) {
    def run(): Unit = test

    case Test1 extends Tests({
      // The following will result in a type error because the two arms of the
      // `if` expression don't agree with each other — one has type `Bool`, the
      // other has type `Bool → Bool`.
      val term =
        Ann(
          Abs("b",
            If(
              Var("b"),
              False,
              Ann(Abs("_", True), Fn(Bool, Bool))
            ),
          ),
          Fn(Bool, Bool),
        )

      val res = Bidi.infer(term)

      println(s"RESULT: ${res.map(_.toString).merge}")
    })

    case Test2 extends Tests({
      // Can't use `True` as a function.
      val term = App(True, False)
      val res = Bidi.infer(term)
      println(s"RESULT: ${res.map(_.toString).merge}")
    })

    case Test3 extends Tests({
      // A bool can't have function type.
      val term = Ann(True, Fn(Bool, Bool))
      val res = Bidi.infer(term)
      println(s"RESULT: ${res.map(_.toString).merge}")
    })

    case Test4 extends Tests({
      val not = Ann(Abs("x", If(Var("x"), False, True)), Fn(Bool, Bool))
      val res = Bidi.infer(not)
      println(s"RESULT: ${res.map(_.toString).merge}")
    })

    case Test5 extends Tests({
      // Function composition. See "Figure 1" in the paper.
      val compose = Ann(
        Abs("f", Abs("g", Abs("b", App(Var("g"), App(Var("f"), Var("b")))))),
        Fn(Fn(Bool, Bool), Fn(Fn(Bool, Bool), Fn(Bool, Bool))),
      )
      val res = Bidi.infer(compose)
      println(s"RESULT: ${res.map(_.toString).merge}")
    })

    case Test6 extends Tests({
      val lambda = Abs("_", True)
      val res = Bidi.infer(lambda)
      println(s"RESULT: ${res.map(_.toString).merge}")
    })
  }
}
