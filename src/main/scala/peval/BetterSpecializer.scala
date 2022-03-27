package peval

import cats.data.State
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

  def specialize(program: Program): Program = {
    def go(expr: Expr, env: Env): State[Program.Defs, Expr] = {
      //println(s"specializing: $expr")
      val a: State[Program.Defs, Expr] = expr match {
        case Const(_) => State.pure(expr)

        case Var(v) =>
          env.bindings.get(v) match {
            case Some(e) => State.pure(Const(e))
            case None => State.pure(expr)
          }

        case Prim(op, a, b) =>
          (go(a, env), go(b, env)).mapN {
            case (Const(a), Const(b)) => Const(op(a, b))
            case (av, bv) => Prim(op, av, bv)
          }

        case If(cond, condT, condF) =>
          go(cond, env).flatMap {
            case Const(Val.B(true)) => go(condT, env)
            case Const(Val.B(false)) => go(condF, env)
            case scond => (go(condT, env), go(condF, env)).mapN(If(scond, _, _))
          }

        case Apply(fn, args) =>
          // look up function
          program.defs.get(fn) match {
            case None => sys.error(s"undefined function: $fn")
            case Some(Def(argNames, body)) =>
              // partially evaluate arguments
              args.traverse(go(_, env)).flatMap { argVals =>
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
                  go(body, Env(staticArgs.toMap))
                } else {
                  // generate a function name
                  val genFn = generateFn(fn, staticArgs)

                  for {
                    defs <- State.get[Program.Defs]
                    _ <- defs.get(genFn) match {
                      case Some(_) =>
                        // We've already specialized this.
                        ().pure[State[Program.Defs, *]]

                      case None =>
                        for {
                          // Since functions can be recursive, we need to
                          // put a placeholder of the function itself in
                          // the fun env before specializing its body.
                          //
                          // Using null seems like a hack, but for the moment
                          // I'm just following the paper, and they did the
                          // same, only using Haskell's undefined instead.
                          _ <- State.set[Program.Defs](defs + (genFn -> null))

                          // Now we can specialize the body over the arguments
                          // that we already know.
                          pbody <- go(body, Env(staticArgs.toMap))

                          // Finally, generate a new function, which receives
                          // just the dynamic subset of the arguments that the
                          // original function expected. Its body is the
                          // original specialized body.
                          //
                          // This part will also override the null entry we
                          // created above.
                          _ <- State.modify[Program.Defs] { defs =>
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

      a.map { residue =>
        println(s"SPECIALIZED:\n" +
          s"              env: $env\n" +
          s"          program: $expr\n" +
          s"          residue: $residue")
        residue
      }
    }

    go(program.main, Env.empty)
      .run(Map.empty)
      .map((Program.apply _).tupled)
      .value
  }

  private def generateFn(fn: String, staticArgs: List[(String, Val)]): String =
    "fn" + (fn + staticArgs).hashCode
}
