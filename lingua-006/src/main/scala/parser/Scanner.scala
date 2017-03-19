package codecamp
package parser

import Character.{ digit, isLetter, isDigit, isWhitespace }

sealed trait Token

object Token {
  case object IF extends Token
  case object THEN extends Token
  case object ELSE extends Token
  case object FN extends Token
  case object DARROW extends Token
  case object LET extends Token
  case object IN extends Token
  case object END extends Token
  case object VAL extends Token
  case object EQUAL extends Token
  case object TRUE extends Token
  case object FALSE extends Token
  case object LPAREN extends Token
  case object RPAREN extends Token
  case object ADD extends Token
  case object SUB extends Token
  case class INT(value: Int) extends Token
  case class VAR(value: String) extends Token
}

object Scanner {
  private type Reader[C, S] = S => Option[(C, S)]

  def scan(s: String): List[Token] = {
    val (tokens, stream) = consume(scanSingle(fromString), s)

    fromString(skipWhiteSpace(fromString, stream)) match {
      case Some((c, s)) => throw new RuntimeException(s"unpexpected character: '$c'")
      case None => tokens
    }
  }

  private val fromString: Reader[Char, String] = {
    string =>
      string.size match {
        case 0 => None
        case 1 => Some(string.head -> "")
        case _ => Some(string.head -> string.substring(1))
      }
  }

  private def consume[C, S](reader: Reader[C, S], stream: S): (List[C], S) = {
    @annotation.tailrec
    def recur(stream: S, result: Vector[C]): (List[C], S) = {
      reader(stream) match {
        case None => result.toList -> stream
        case Some((c, s)) => recur(s, result :+ c)
      }
    }

    recur(stream, Vector.empty)
  }

  @annotation.tailrec
  private def skipWhiteSpace[S](reader: Reader[Char, S], stream: S): S = {
    reader(stream) match {
      case Some((c, stream)) if isWhitespace(c) => skipWhiteSpace(reader, stream)
      case _ => stream
    }
  }

  private def scanSingle[S](nextChar: Reader[Char, S]): Reader[Token, S] = { stream =>
    val s = skipWhiteSpace(nextChar, stream)
    scanNumber(nextChar)(s) orElse
    scanIdent(nextChar)(s) orElse
    scanSymbol(nextChar)(s)
  }

  private def scanSymbol[S](nextChar: Reader[Char, S]): Reader[Token, S] = { stream =>
    nextChar(stream) match {
      case Some(('=', stream)) =>
        nextChar(stream) match {
          case None => None
          case Some(('>', stream)) => Some(Token.DARROW -> stream)
          case Some((_, stream)) => Some(Token.EQUAL -> stream)
        }
      case Some(('(', stream)) =>  Some(Token.LPAREN -> stream)
      case Some((')', stream)) =>  Some(Token.RPAREN -> stream)
      case Some(('+', stream)) =>  Some(Token.ADD -> stream)
      case Some(('-', stream)) =>  Some(Token.SUB -> stream)
      case _ => None
    }
  }

  private def scanIdent[S](nextChar: Reader[Char, S]): Reader[Token, S] = { stream =>
    @annotation.tailrec
    def recur(stream: S, result: String): Option[(String, S)] = {
      nextChar(stream) match {
        case Some((c, s)) if isLetter(c) => recur(s, result + c.toString)
        case _ if result == "" => None
        case _ => Some(result -> stream)
      }
    }

    recur(stream, "").map {
      case ("if", s) => Token.IF -> s
      case ("then", s) => Token.THEN -> s
      case ("else", s) => Token.ELSE -> s
      case ("fn", s) => Token.FN -> s
      case ("let", s) => Token.LET -> s
      case ("in", s) => Token.IN -> s
      case ("end", s) => Token.END -> s
      case ("val", s) => Token.VAL -> s
      case ("true", s) => Token.TRUE -> s
      case ("false", s) => Token.FALSE -> s
      case (ident, s) => Token.VAR(ident) -> s
    }
  }

  private def scanNumber[S](nextChar: Reader[Char, S]): Reader[Token, S] = { stream =>
    @annotation.tailrec
    def recur(stream: S, result: Option[Int]): Option[(Int, S)] = {
      scanDigit(nextChar)(stream) match {
        case Some((n, s)) => recur(s, Some(result.getOrElse(0) * 10 + n))
        case None => result.map(_ -> stream)
      }
    }

    recur(stream, None).map {
      case (n, s) => Token.INT(n) -> s
    }
  }

  private def scanDigit[S](nextChar: Reader[Char, S]): Reader[Int, S] = { stream =>
    nextChar(stream).flatMap {
      case p @ (c, s) if isDigit(c) =>
        Some(digit(c, 10) -> s)
      case _ => None
    }
  }
}
