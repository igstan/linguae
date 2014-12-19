package toy

object Interpreter {
  def eval(ast: Node): Value = ast match {
    case Num(n) => Value.Num(n)
  }
}
