package linguae
package test

final class ImperativeShuntingYardParserSuite extends munit.FunSuite {
  private val operatorTable = Map(
    "^" -> Fixity.R(10),
    "*" -> Fixity.L(5),
    "/" -> Fixity.L(5),
    "+" -> Fixity.L(0),
    "-" -> Fixity.L(0),
  )

  private val parser = ImperativeShuntingYardParser(operatorTable)

  private val exprs = List(
    "1 + 2",
    "(1 + 2) * 3",
    "4 + -2 - 1",
    "4 * 2 / 1",
    "1 + 2 * 3",
    "4 + 4 * 2 / (1 - 5)",
    "4 + (4 + 2) / (1 - 5)",
  )

  exprs.foreach { expr =>
    test(s"parses: $expr") {
      val result = Token
        .parser
        .consume(expr)
        .map(parser.parse)
        .toOption
        .get
        .pretty(operatorTable)

      assertEquals(result, expr)
    }
  }
}
