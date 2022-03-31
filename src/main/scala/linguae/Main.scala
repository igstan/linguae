package linguae

object Main {
  private val operatorTable = Map(
    "^" -> Fixity.R(6),
    "*" -> Fixity.L(3),
    "/" -> Fixity.L(3),
    "+" -> Fixity.L(0),
    "-" -> Fixity.L(0),
  )

  private val exprs = List(
    "         4 + -2 - 1",
    "          4 * 2 / 1",
    "          1 + 2 * 3",
    "4 + 4 * 2 / (1 - 5)",
  )

  def main(args: Array[String]): Unit = {
    println()
    println("FunctionalShuntingYardParser")
    println("----------------------------")
    samples(FunctionalShuntingYardParser(operatorTable))
    println()
    println("ImperativeShuntingYardParser")
    println("----------------------------")
    samples(ImperativeShuntingYardParser(operatorTable))
    println()
  }

  private def samples(parser: ExprParser): Unit =
    exprs.foreach { expr =>
      println {
        expr + "   →   " + Token
          .parser
          .consume(expr)
          .map(parser.parse)
          .toOption
          .get
          .pretty(operatorTable)
      }
    }
}
