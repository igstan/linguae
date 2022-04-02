package linguae
package test

final class FunctionalShuntingYardParserSuite extends ExprParserSuite {
  override def parser: ExprParser =
    FunctionalShuntingYardParser(operatorTable)
}
