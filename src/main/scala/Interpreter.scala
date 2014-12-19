package toy

object Interpreter {
  def eval(ast: Node): Result = ast match {
    case Num(n) => Result.Success(Value.Num(n))
    case Add(l, r) => arith(l, r, _ + _)
    case Sub(l, r) => arith(l, r, _ - _)
    case Mul(l, r) => arith(l, r, _ * _)
    case Div(l, r) => arith(l, r, _ / _)
    case If(cond, yes, no) =>
      eval(cond) match {
        case f: Result.Failure => f
        case Result.Success(Value.Num(0)) => eval(no)
        case Result.Success(Value.Num(_)) => eval(yes)
        case Result.Success(_: Value.Fun) =>
          Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) => Result.Success(Value.Fun(param, body))
  }

  private def arith(l: Node, r: Node, op: (Int, Int) => Int): Result = {
    eval(l) match {
      case f: Result.Failure => f
      case Result.Success(_: Value.Fun) =>
        Result.Failure("Left operand is not a number")
      case Result.Success(Value.Num(l)) =>
        eval(r) match {
          case f: Result.Failure => f
          case Result.Success(_: Value.Fun) =>
            Result.Failure("Right operand is not a number")
          case Result.Success(Value.Num(r)) =>
            Result.Success(Value.Num(op(l, r)))
        }
    }
  }
}
