package linguae
package test

final class ImperativeShuntingYardParserSuite extends ExprParserSuite {
  override def parser: ExprParser =
    ImperativeShuntingYardParser(operatorTable)
}
