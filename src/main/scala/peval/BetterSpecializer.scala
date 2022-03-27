package peval

import cats.data.ReaderWriterState
import cats.implicits._
import peval.Expr.{ Apply, Const, If, Prim, Var }

object BetterSpecializer {
  final case class Env(bindings: Map[String, Val]) extends AnyVal {
    override def toString: String =
      bindings.map { case (k, v) => s"$k → $v" }.mkString("[", ", ", "]")
  }

  object Env {
    val empty: Env = Env(Map.empty)
  }

  final case class Residue(logs: Log, program: Program)

  final case class Stage(env: Env, program: Expr, residue: Expr) {
    override def toString: String =
      List(
        s"environ: $env",
        s"program: $program",
        s"residue: $residue",
      ).mkString("\n")
  }

  private type Log = Vector[Stage]
  private type Specialized[A] = ReaderWriterState[Env, Log, Program.Defs, A]

  private val Specialized = ReaderWriterState
  private val env = Specialized.ask[Env, Log, Program.Defs]

  def specialize(program: Program): Residue = {
    def go(expr: Expr): Specialized[Expr] = {
      val residue: Specialized[Expr] = expr match {
        case Const(_) => Specialized.pure(expr)

        case Var(v) =>
          env.flatMap { env =>
            env.bindings.get(v) match {
              case Some(e) => Specialized.pure(Const(e))
              case None => Specialized.pure(expr)
            }
          }

        case Prim(op, a, b) =>
          (go(a), go(b)).mapN {
            case (Const(a), Const(b)) => Const(op(a, b))
            case (Const(Val.I(0)), a) if op == Op.Add => a
            case (a, Const(Val.I(0))) if op == Op.Add => a
            case (a, Const(Val.I(0))) if op == Op.Sub => a
            case (Const(Val.I(1)), a) if op == Op.Mul => a
            case (a, Const(Val.I(1))) if op == Op.Mul => a
            case (av, bv) => Prim(op, av, bv)
          }

        case If(cond, condT, condF) =>
          go(cond).flatMap {
            case Const(Val.B(true)) => go(condT)
            case Const(Val.B(false)) => go(condF)
            case scond => (go(condT), go(condF)).mapN(If(scond, _, _))
          }

        case Apply(fn, args) =>
          // look up function
          program.defs.get(fn) match {
            case None => sys.error(s"undefined function: $fn")
            case Some(Def(argNames, body)) =>
              // partially evaluate arguments
              args.traverse(go).flatMap { argVals =>
                // partition arguments into static and dynamic
                val (staticArgs, dynamicArgs) =
                  // It's important to use parMapN here instead of mapN, so
                  // that we get a zipped result instead of a cartesian
                  // product.
                  (argNames, argVals).parMapN { (argName, argVal) =>
                    argVal match {
                      case Const(const) => Left((argName, const))
                      case _ => Right((argName, argVal))
                    }
                  }
                  .separate

                if (dynamicArgs.isEmpty) {
                  // All arguments are statically known; we can completely
                  // inline the entire function body.
                  go(body).local(_ => Env(staticArgs.toMap))
                } else {
                  // generate a function name
                  val genFn = generateFn(fn, staticArgs)

                  for {
                    defs <- Specialized.get[Env, Log, Program.Defs]
                    _ <- defs.get(genFn) match {
                      case Some(_) =>
                        // We've already specialized this.
                        ().pure[Specialized]

                      case None =>
                        for {
                          // Since functions can be recursive, we need to
                          // put a placeholder of the function itself in
                          // the fun env before specializing its body.
                          //
                          // Using null seems like a hack, but for the moment
                          // I'm just following the paper, and they did the
                          // same, only using Haskell's undefined instead.
                          _ <- Specialized.set[Env, Log, Program.Defs](defs + (genFn -> null))

                          // Now we can specialize the body over the arguments
                          // that we already know.
                          pbody <- go(body).local[Env](_ => Env(staticArgs.toMap))

                          // Finally, generate a new function, which receives
                          // just the dynamic subset of the arguments that the
                          // original function expected. Its body is the
                          // original specialized body.
                          //
                          // This part will also override the null entry we
                          // created above.
                          _ <- Specialized.modify[Env, Log, Program.Defs] { defs =>
                            val argNames = dynamicArgs.map(_._1)
                            defs + (genFn -> Def(argNames, pbody))
                          }
                        } yield ()
                    }
                  } yield {
                    // Call the generated function using the dynamic subset of
                    // the arguments.
                    Apply(genFn, dynamicArgs.map(_._2))
                  }
                }
              }
          }
      }

      for {
        r <- residue
        e <- env
        _ <- log(e, expr, r)
      } yield {
        r
      }
    }

    go(program.main)
      .run(Env.empty, Map.empty)
      .map { case (logs, defs, main) => Residue(logs, Program(defs, main)) }
      .value
  }

  private def log(environ: Env, program: Expr, residue: Expr): Specialized[Unit] =
    Specialized.tell(Vector(Stage(environ, program, residue)))

  private def generateFn(fn: String, staticArgs: List[(String, Val)]): String =
    "fn" + (fn + staticArgs).hashCode
}
