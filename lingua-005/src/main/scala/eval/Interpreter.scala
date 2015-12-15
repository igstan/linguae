package ro.igstan.debugger
package eval

import display.HtmlRenderer
import syntax.Term

object Interpreter {
  def eval(term: Term, env: Env)(kont: Kont): Resumption = {
    def loop(term: Term, env: Env, prevResult: Option[Result], kont: Kont): Resumption = {
      term match {
        case Term.INT(value) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing NUM: $term")
            kont(Resumption.Done(Right(Value.Num(value))))
          }
        case Term.BOOL(value) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing BOOL: $term")
            kont(Resumption.Done(Right(Value.Bool(value))))
          }
        case Term.FN(param, body) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing FUN: $term")
            kont(Resumption.Done(Right(Value.Fun(param, body, env))))
          }
        case Term.ADD(a, b) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing ADD: $term")
            Resumption.Next {
              loop(a, env, prevResult, {
                case Resumption.Next(r) => r.next()
                case Resumption.Done(v) =>
                  v match {
                    case Left(error) => kont(Resumption.Done(Left(error)))
                    case Right(aValue @ Value.Num(a)) =>
                      Resumption.Next {
                        loop(b, env, Some(v), {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) =>
                            Resumption.Next(Resumption(env, term.id, Some(v)) { () =>
                              v match {
                                case Left(error) => kont(Resumption.Done(Left(error)))
                                case Right(Value.Num(b)) => kont(Resumption.Done(Right(Value.Num(a + b))))
                                case Right(_) => kont(Resumption.Done(Left("non-number given as right operand to +")))
                              }
                            })
                        })
                      }
                    case Right(_) => kont(Resumption.Done(Left("non-number given as left operand to +")))
                  }
              })
            }
          }
        case Term.SUB(a, b) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing SUB: $term")
            Resumption.Next {
              loop(a, env, prevResult, {
                case Resumption.Next(r) => r.next()
                case Resumption.Done(v) =>
                  v match {
                    case Left(error) => kont(Resumption.Done(Left(error)))
                    case Right(Value.Num(a)) =>
                      Resumption.Next {
                        loop(b, env, Some(v), {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) =>
                            Resumption.Next(Resumption(env, term.id, Some(v)) { () =>
                              v match {
                                case Left(error) => kont(Resumption.Done(Left(error)))
                                case Right(Value.Num(b)) => kont(Resumption.Done(Right(Value.Num(a - b))))
                                case Right(_) => kont(Resumption.Done(Left("non-number given as right operand to +")))
                              }
                            })
                        })
                      }
                    case Right(_) => kont(Resumption.Done(Left("non-number given as left operand to +")))
                  }
              })
            }
          }
        case Term.VAR(name) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing VAR: $term")
            kont {
              env.get(name) match {
                case None => Resumption.Done(Left(s"unbound identifier: $name"))
                case Some(v) => Resumption.Done(Right(v))
              }
            }
          }
        case Term.APP(fn, arg) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing APP: $term")
            Resumption.Next {
              loop(fn, env, prevResult, {
                case Resumption.Next(r) => r.next()
                case Resumption.Done(v) =>
                  v match {
                    case Left(error) => kont(Resumption.Done(Left(error)))
                    case Right(Value.Num(_)) => kont(Resumption.Done(Left("number in function position")))
                    case Right(Value.Bool(_)) => kont(Resumption.Done(Left("boolean in function position")))
                    case Right(Value.Fun(param, body, closure)) =>
                      Resumption.Next {
                        loop(arg, env, Some(v), {
                          case Resumption.Next(r) => Resumption.Next(r)
                          case Resumption.Done(v) =>
                            Resumption.Next(Resumption(env, term.id, Some(v)) { () =>
                              v match {
                                case Left(error) => kont(Resumption.Done(Left(error)))
                                case Right(arg) =>
                                  Resumption.Next {
                                    loop(body, closure.set(param, arg), Some(v), kont)
                                  }
                              }
                            })
                        })
                      }
                  }
              })
            }
          }
        case Term.IF(test, yes, no) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing IF: $term")
            Resumption.Next {
              loop(test, env, prevResult, {
                case Resumption.Next(r) => r.next()
                case Resumption.Done(v) =>
                  v match {
                    case Left(error) => kont(Resumption.Done(Left(error)))
                    case Right(_: Value.Num) => kont(Resumption.Done(Left("number in if predicate position")))
                    case Right(_: Value.Fun) => kont(Resumption.Done(Left("function in if predicate position")))
                    case Right(Value.Bool(test)) =>
                      Resumption.Next {
                        loop(if (test) yes else no, env, Some(v), {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) => kont(Resumption.Done(v))
                        })
                      }
                  }
              })
            }
          }
        case Term.LET(binding, value, body) =>
          Resumption(env, term.id, prevResult) { () =>
            println(s"Executing LET: $term")
            Resumption.Next {
              loop(value, env, prevResult, {
                case Resumption.Next(r) => r.next()
                case Resumption.Done(v) =>
                  v match {
                    case Left(error) => kont(Resumption.Done(Left(error)))
                    case Right(valueResult) =>
                      val extendedEnv = env.set(binding, valueResult)

                      Resumption.Next {
                        loop(body, extendedEnv, Some(v), {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) => kont(Resumption.Done(v))
                        })
                      }
                  }
              })
            }
          }
      }
    }

    loop(term, env, None, kont)
  }
}
