package ro.igstan.debugger
package eval

import syntax.Term

object Interpreter {
  def eval(term: Term, env: Env)(kont: Kont): Resumption = {
    term match {
      case Term.Num(value) =>
        Resumption(env) { () =>
          println(s"Executing NUM: $term")
          kont(Resumption.Done(Right(Value.Num(value))))
        }
      case Term.Fun(param, body) =>
        Resumption(env) { () =>
          println(s"Executing FUN: $term")
          kont(Resumption.Done(Right(Value.Fun(param, body, env))))
        }
      case Term.Var(name) =>
        Resumption(env) { () =>
          println(s"Executing VAR: $term")
          kont {
            env.get(name) match {
              case None => Resumption.Done(Left(s"unbound identifier: $name"))
              case Some(v) => Resumption.Done(Right(v))
            }
          }
        }
      case Term.App(fn, arg) =>
        Resumption(env) { () =>
          println(s"Executing APP: $term")
          Resumption.Next {
            eval(fn, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(Value.Num(_)) => kont(Resumption.Done(Left("number in function position")))
                  case Right(Value.Native(native)) =>
                    Resumption.Next {
                      eval(arg, env) {
                        case Resumption.Next(r) => r.next()
                        case Resumption.Done(v) =>
                          Resumption.Next(Resumption(Env.empty) { () =>
                            v match {
                              case Left(error) => kont(Resumption.Done(Left(error)))
                              case Right(arg) => kont(Resumption.Done(native(arg)))
                            }
                          })
                      }
                    }
                  case Right(fn @ Value.Fun(param, body, closure)) =>
                    Resumption.Next {
                      eval(arg, env) {
                        case Resumption.Next(r) => Resumption.Next(r)
                        case Resumption.Done(v) =>
                          Resumption.Next(Resumption(closure) { () =>
                            v match {
                              case Left(error) => kont(Resumption.Done(Left(error)))
                              case Right(arg) =>
                                Resumption.Next {
                                  eval(body, closure.set(param, arg))(kont)
                                }
                            }
                          })
                      }
                    }
                }
            }
          }
        }
    }
  }
}
