package toy

object Interpreter {
  def eval(ast: Node, env: Environment): Result = ast match {
    case Num(n) => Result.Success(Value.Num(n))
    case Add(l, r) => arith(l, r, env, _ + _)
    case Sub(l, r) => arith(l, r, env, _ - _)
    case Mul(l, r) => arith(l, r, env, _ * _)
    case Div(l, r) => arith(l, r, env, _ / _)
    case If(cond, yes, no) =>
      eval(cond, env) match {
        case f: Result.Failure => f
        case Result.Success(Value.Num(0)) => eval(no, env)
        case Result.Success(Value.Num(_)) => eval(yes, env)
        case Result.Success(_: Value.Fun) =>
          Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) => Result.Success(Value.Fun(param, body))
    case App(fn, arg) =>
      eval(fn, env) match {
        case f: Result.Failure => f
        case Result.Success(Value.Num(_)) =>
          Result.Failure("Number in function application position.")
        case Result.Success(Value.Fun(param, body)) =>
          eval(arg, env) match {
            case f: Result.Failure => f
            case Result.Success(value) =>
              val augmentedEnv = env.set(param, value)
              eval(body, augmentedEnv)
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

  private def arith(l: Node, r: Node, env: Environment, op: (Int, Int) => Int): Result = {
    eval(l, env) match {
      case f: Result.Failure => f
      case Result.Success(_: Value.Fun) =>
        Result.Failure("Left operand is not a number")
      case Result.Success(Value.Num(l)) =>
        eval(r, env) match {
          case f: Result.Failure => f
          case Result.Success(_: Value.Fun) =>
            Result.Failure("Right operand is not a number")
          case Result.Success(Value.Num(r)) =>
            Result.Success(Value.Num(op(l, r)))
        }
    }
  }
}
