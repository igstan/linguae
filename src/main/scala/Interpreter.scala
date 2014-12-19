package toy

object Interpreter {
  def eval(ast: Node): Value = ast match {
    case Num(n) => Value.Num(n)
    case Add(l, r) => arith(l, r, _ + _)
    case Sub(l, r) => arith(l, r, _ - _)
    case Mul(l, r) => arith(l, r, _ * _)
    case Div(l, r) => arith(l, r, _ / _)
  }

  private def arith(l: Node, r: Node, op: (Int, Int) => Int): Value = {
    eval(l) match {
      case Value.Num(l) =>
        eval(r) match {
          case Value.Num(r) => Value.Num(op(l, r))
        }
    }
  }
}
