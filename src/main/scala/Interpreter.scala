package toy

object Interpreter {
  def eval(ast: Node, env: Environment): Result[Value] = ast match {
    case Num(n) => Result.Success(Value.Num(n))
    case Add(l, r) => arith(l, r, env, _ + _)
    case Sub(l, r) => arith(l, r, env, _ - _)
    case Mul(l, r) => arith(l, r, env, _ * _)
    case Div(l, r) => arith(l, r, env, _ / _)
    case If(cond, yes, no) =>
      eval(cond, env).flatMap {
        case Value.Num(0) => eval(no, env)
        case Value.Num(_) => eval(yes, env)
        case _: Value.Fun => Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) => Result.Success(Value.Fun(param, body, closure = env))
    case App(fn, arg) =>
      eval(fn, env).flatMap {
        case Value.Num(_) => Result.Failure("Number in function application position.")
        case Value.Fun(param, body, closure) =>
          eval(arg, env).flatMap {
            case value => eval(body, closure.set(param, value))
          }
      }
    case Ref(name) =>
      env.get(name).map(Result.Success(_)).getOrElse {
        Result.Failure(s"Unbound identifier: $name")
      }
    case Let(name, value, body) =>
      val desugared = App(Fun(name, body), value)
      eval(desugared, env)
  }

  private def arith(l: Node, r: Node, env: Environment, op: (Int, Int) => Int): Result[Value] = {
    eval(l, env).flatMap {
      case _: Value.Fun => Result.Failure("Left operand is not a number")
      case Value.Num(l) =>
        eval(r, env).flatMap {
          case _: Value.Fun => Result.Failure("Right operand is not a number")
          case Value.Num(r) => Result.Success(Value.Num(op(l, r)))
        }
    }
  }
}
