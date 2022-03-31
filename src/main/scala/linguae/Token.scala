package linguae

enum Token {
  case Const(value: Int)
  case BinOp(value: String)
  case ParenL
  case ParenR
}

object Token {
  def parser: Parser[List[Token]] = {
    import Parser.*

    def token[A](parser: Parser[A]): Parser[A] = spaces *> parser <* spaces
    def spaces: Parser[Unit] = satisfy(_.isWhitespace).optionalMany.void
    def lparen: Parser[Token] = char('(').as(ParenL)
    def rparen: Parser[Token] = char(')').as(ParenR)

    def number: Parser[Token] =
      for {
        n <- char('-').optional.map(_.isDefined)
        d <- satisfy(_.isDigit).many.map(_.mkString.toInt)
      } yield {
        Const(if (n) -d else d)
      }

    def operator: Parser[Token] =
      Set('+', '-', '*', '/', '^')
        .map(char)
        .reduce(_ | _)
        .map(op => BinOp(op.toString))

    token(lparen | rparen | number | operator).optionalMany
  }
}
