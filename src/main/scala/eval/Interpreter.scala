package ro.igstan.debugger
package eval

import display.HtmlRenderer
import syntax.Term

object Interpreter {
  def eval(term: Term, env: Env)(kont: Kont): Resumption = {
    term match {
      case Term.INT(value) =>
        Resumption(env) { () =>
          println(s"Executing NUM: $term")
          kont(Resumption.Done(Right(Value.Num(value))))
        }
      case Term.BOOL(value) =>
        Resumption(env) { () =>
          println(s"Executing BOOL: $term")
          kont(Resumption.Done(Right(Value.Bool(value))))
        }
      case Term.FN(param, body) =>
        Resumption(env) { () =>
          println(s"Executing FUN: $term")
          kont(Resumption.Done(Right(Value.Fun(param, body, env))))
        }
      case Term.ADD(a, b) =>
        Resumption(env) { () =>
          println(s"Executing ADD: $term")
          Resumption.Next {
            eval(a, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(Value.Num(a)) =>
                    Resumption.Next {
                      eval(b, env) {
                        case Resumption.Next(r) => r.next()
                        case Resumption.Done(v) =>
                          Resumption.Next(Resumption(env) { () =>
                            v match {
                              case Left(error) => kont(Resumption.Done(Left(error)))
                              case Right(Value.Num(b)) => kont(Resumption.Done(Right(Value.Num(a + b))))
                              case Right(_) => kont(Resumption.Done(Left("non-number given as right operand to +")))
                            }
                          })
                      }
                    }
                  case Right(_) => kont(Resumption.Done(Left("non-number given as left operand to +")))
                }
            }
          }
        }
      case Term.SUB(a, b) =>
        Resumption(env) { () =>
          println(s"Executing SUB: $term")
          Resumption.Next {
            eval(a, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(Value.Num(a)) =>
                    Resumption.Next {
                      eval(b, env) {
                        case Resumption.Next(r) => r.next()
                        case Resumption.Done(v) =>
                          Resumption.Next(Resumption(env) { () =>
                            v match {
                              case Left(error) => kont(Resumption.Done(Left(error)))
                              case Right(Value.Num(b)) => kont(Resumption.Done(Right(Value.Num(a - b))))
                              case Right(_) => kont(Resumption.Done(Left("non-number given as right operand to +")))
                            }
                          })
                      }
                    }
                  case Right(_) => kont(Resumption.Done(Left("non-number given as left operand to +")))
                }
            }
          }
        }
      case Term.VAR(name) =>
        Resumption(env) { () =>
          println(s"Executing VAR: $term")
          kont {
            env.get(name) match {
              case None => Resumption.Done(Left(s"unbound identifier: $name"))
              case Some(v) => Resumption.Done(Right(v))
            }
          }
        }
      case Term.APP(fn, arg) =>
        Resumption(env) { () =>
          println(s"Executing APP: $term")
          Resumption.Next {
            eval(fn, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(Value.Num(_)) => kont(Resumption.Done(Left("number in function position")))
                  case Right(Value.Bool(_)) => kont(Resumption.Done(Left("boolean in function position")))
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
      case Term.IF(test, yes, no) =>
        Resumption(env) { () =>
          println(s"Executing IF: $term")
          Resumption.Next {
            eval(test, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(_: Value.Num) => kont(Resumption.Done(Left("number in if predicate position")))
                  case Right(_: Value.Native) => kont(Resumption.Done(Left("function in if predicate position")))
                  case Right(_: Value.Fun) => kont(Resumption.Done(Left("function in if predicate position")))
                  case Right(Value.Bool(test)) =>
                    Resumption.Next(Resumption(env) { () =>
                      Resumption.Next {
                        eval(if (test) yes else no, env) {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) => kont(Resumption.Done(v))
                        }
                      }
                    })
                }
            }
          }
        }
      case Term.LET(binding, value, body) =>
        Resumption(env) { () =>
          println(s"Executing LET: $term")
          Resumption.Next {
            eval(value, env) {
              case Resumption.Next(r) => r.next()
              case Resumption.Done(v) =>
                v match {
                  case Left(error) => kont(Resumption.Done(Left(error)))
                  case Right(value) =>
                    val extendedEnv = env.set(binding, value)

                    Resumption.Next(Resumption(extendedEnv) { () =>
                      Resumption.Next {
                        eval(body, extendedEnv) {
                          case Resumption.Next(r) => r.next()
                          case Resumption.Done(v) => kont(Resumption.Done(v))
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
