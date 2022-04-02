package linguae

trait ExprParser {
  def parse(tokens: List[Token]): Expr
}
