package linguae

object Main {
  def main(args: Array[String]): Unit = {
    val operatorTable = Map(
      "^" -> Fixity.R(10),
      "*" -> Fixity.L(5),
      "/" -> Fixity.L(5),
      "+" -> Fixity.L(0),
      "-" -> Fixity.L(0),
    )

    val parser = FunctionalShuntingYardParser(operatorTable = operatorTable)
    val exprs = List(
      "         4 + -2 - 1",
      "          4 * 2 / 1",
      "          1 + 2 * 3",
      "4 + 4 * 2 / (1 - 5)",
    )

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
}
