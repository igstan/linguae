package linguae
package test

abstract class ExprParserSuite extends munit.FunSuite {
  def parser: ExprParser

  protected val operatorTable = Map(
    "^" -> Fixity.R(6),
    "*" -> Fixity.L(3),
    "/" -> Fixity.L(3),
    "+" -> Fixity.L(0),
    "-" -> Fixity.L(0),
  )

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
