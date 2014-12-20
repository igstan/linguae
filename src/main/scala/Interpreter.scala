package toy

object Interpreter {
  def eval(ast: Node, env: Environment): Result[Evaluation] = ast match {
    case Num(n) => Result.Success(Evaluation(Value.Num(n), env))
    case Add(l, r) => arith(l, r, env, _ + _)
    case Sub(l, r) => arith(l, r, env, _ - _)
    case Mul(l, r) => arith(l, r, env, _ * _)
    case Div(l, r) => arith(l, r, env, _ / _)
    case If(cond, yes, no) =>
      eval(cond, env).flatMap {
        case Evaluation(Value.Num(0), env) => eval(no, env)
        case Evaluation(Value.Num(_), env) => eval(yes, env)
        case Evaluation(_: Value.Fun, env) => Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) =>
      Result.Success(Evaluation(Value.Fun(param, body, closure = env), env))
    case App(fn, arg) =>
      eval(fn, env).flatMap {
        case Evaluation(Value.Num(_), env) => Result.Failure("Number in function application position.")
        case Evaluation(Value.Fun(param, body, closure), env) =>
          eval(arg, env).flatMap {
            case Evaluation(value, env) => eval(body, closure.set(param, value))
          }
      }
    case Ref(name) =>
      env.get(name).map(v => Result.Success(Evaluation(v, env))).getOrElse {
        Result.Failure(s"Unbound identifier: $name")
      }
    case Let(name, value, body) =>
      val desugared = App(Fun(name, body), value)
      eval(desugared, env)
    case Seq(a, b) =>
      eval(a, env).flatMap {
        case Evaluation(_, env) => eval(b, env)
      }
    case Set(name, value) =>
      eval(value, env).flatMap {
        case Evaluation(value, env) =>
          val updatedEnv = env.set(name, value)
          Result.Success(Evaluation(value, updatedEnv))
      }
  }

  private def arith(l: Node, r: Node, env: Environment, op: (Int, Int) => Int): Result[Evaluation] = {
    eval(l, env).flatMap {
      case Evaluation(_: Value.Fun, env) => Result.Failure("Left operand is not a number")
      case Evaluation(Value.Num(l), env) =>
        eval(r, env).flatMap {
          case Evaluation(_: Value.Fun, env) => Result.Failure("Right operand is not a number")
          case Evaluation(Value.Num(r), env) => Result.Success(Evaluation(Value.Num(op(l, r)), env))
        }
    }
  }
}
