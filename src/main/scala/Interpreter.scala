package toy

object Interpreter {
  def eval(ast: Node): Value = ast match {
    case Num(n) => Value.Num(n)
    case Add(l, r) =>
      eval(l) match {
        case Value.Num(l) =>
          eval(r) match {
            case Value.Num(r) => Value.Num(l + r)
          }
      }
    case Sub(l, r) =>
      eval(l) match {
        case Value.Num(l) =>
          eval(r) match {
            case Value.Num(r) => Value.Num(l - r)
          }
      }
  }
}
