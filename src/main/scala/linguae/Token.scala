package linguae

sealed trait Token extends Product with Serializable

object Token {
  final case class Const(value: Int) extends Token
  final case class BinOp(value: String) extends Token
  final case object LParen extends Token
  final case object RParen extends Token

  def parser: Parser[List[Token]] = {
    import Parser.*

    def token[A](parser: Parser[A]): Parser[A] = spaces *> parser <* spaces
    def spaces: Parser[Unit] = satisfy(_.isWhitespace).optionalMany.void
    def lparen: Parser[Token] = char('(').as(Token.LParen)
    def rparen: Parser[Token] = char(')').as(Token.RParen)

    def number: Parser[Token] =
      for {
        n <- char('-').optional.map(_.isDefined)
        d <- satisfy(_.isDigit).many.map(_.mkString.toInt)
      } yield {
        Token.Const(if (n) -d else d)
      }

    def operator: Parser[Token] =
      Set('+', '-', '*', '/', '^')
        .map(char)
        .reduce(_ | _)
        .map(op => Token.BinOp(op.toString))

    token(lparen | rparen | number | operator).optionalMany
  }
}
