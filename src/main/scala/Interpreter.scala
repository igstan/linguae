package toy

object Interpreter {
  def eval(ast: Node, env: Environment): Result[Evaluation] = ast match {
    case Num(n) => Result.Success(Evaluation(Value.Num(n)))
    case Add(l, r) => arith(l, r, env, _ + _)
    case Sub(l, r) => arith(l, r, env, _ - _)
    case Mul(l, r) => arith(l, r, env, _ * _)
    case Div(l, r) => arith(l, r, env, _ / _)
    case If(cond, yes, no) =>
      eval(cond, env).flatMap {
        case Evaluation(Value.Num(0)) => eval(no, env)
        case Evaluation(Value.Num(_)) => eval(yes, env)
        case Evaluation(_: Value.Fun) => Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) =>
      Result.Success(Evaluation(Value.Fun(param, body, closure = env)))
    case App(fn, arg) =>
      eval(fn, env).flatMap {
        case Evaluation(Value.Num(_)) => Result.Failure("Number in function application position.")
        case Evaluation(Value.Fun(param, body, closure)) =>
          eval(arg, env).flatMap {
            case Evaluation(value) => eval(body, closure.set(param, value))
          }
      }
    case Ref(name) =>
      env.get(name).map(v => Result.Success(Evaluation(v))).getOrElse {
        Result.Failure(s"Unbound identifier: $name")
      }
    case Let(name, value, body) =>
      val desugared = App(Fun(name, body), value)
      eval(desugared, env)
  }

  private def arith(l: Node, r: Node, env: Environment, op: (Int, Int) => Int): Result[Evaluation] = {
    eval(l, env).flatMap {
      case Evaluation(_: Value.Fun) => Result.Failure("Left operand is not a number")
      case Evaluation(Value.Num(l)) =>
        eval(r, env).flatMap {
          case Evaluation(_: Value.Fun) => Result.Failure("Right operand is not a number")
          case Evaluation(Value.Num(r)) => Result.Success(Evaluation(Value.Num(op(l, r))))
        }
    }
  }
}
