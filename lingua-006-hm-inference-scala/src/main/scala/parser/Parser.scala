package codecamp
package parser

case class UnexpectedEndOfStream(expected: String)
  extends RuntimeException(s"Unexpected end of stream; $expected")

case class ParseError(found: Token, expected: String)
  extends RuntimeException(s"Unexpected token: $found; expected $expected")

object Parser {
  def parse(source: String): Term = {
    parse(Scanner.scan(source))
  }

  def parse(tokens: List[Token]): Term = {
    parseExp(tokens) match {
      case (absyn, Nil) => absyn
      case (_, token :: _) =>
        throw ParseError(token, "INT, TRUE, FALSE, VAR, IF, FN, LET")
    }
  }

  // <ATEXP>
  private def parseAtExp(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.INT(n) :: tokens => INT(n) -> tokens
      case Token.TRUE :: tokens => BOOL(true) -> tokens
      case Token.FALSE :: tokens => BOOL(false) -> tokens
      case Token.VAR(id) :: tokens => VAR(id) -> tokens
      case Token.LET :: tokens => parseLet(tokens)
      case Token.LPAREN :: tokens =>
        val (exp, rest) = parseExp(tokens)
        rest match {
          case Token.RPAREN :: tokens => exp -> tokens
          case token :: _ => throw ParseError(token, "RPAREN")
          case _ => throw UnexpectedEndOfStream("RPAREN")
        }
      case token :: _ => throw ParseError(token, "INT, TRUE, FALSE, VAR, LET, LPAREN")
      case _ => throw UnexpectedEndOfStream("INT, TRUE, FALSE, VAR, LET, LPAREN")
    }
  }

  private def parseLet(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.VAL :: tokens =>
        tokens match {
          case Token.VAR(bindingName) :: tokens =>
            tokens match {
              case Token.EQUAL :: tokens =>
                val (bindingValue, rest) = parseExp(tokens)
                rest match {
                  case Token.IN :: tokens =>
                    val (body, rest) = parseExp(tokens)
                    rest match {
                      case Token.END :: tokens =>
                        LET(bindingName, bindingValue, body) -> tokens
                      case token :: _ => throw ParseError(token, "END")
                      case _ => throw UnexpectedEndOfStream("END")
                    }
                  case token :: _ => throw ParseError(token, "IN")
                  case _ => throw UnexpectedEndOfStream("IN")
                }
              case token :: _ => throw ParseError(token, "EQUAL")
              case _ => throw UnexpectedEndOfStream("EQUAL")
            }
          case token :: _ => throw ParseError(token, "VAR")
          case _ => throw UnexpectedEndOfStream("VAR")
        }
      case token :: _ => throw ParseError(token, "VAL")
      case _ => throw UnexpectedEndOfStream("VAL")
    }
  }

  // <APPEXP>
  private def parseAppExp(tokens: List[Token]): (Term, List[Token]) = {
    @annotation.tailrec
    def recur(tokens: List[Token], absyn: Term): (Term, List[Token]) = {
      // 1 token lookahead
      if (nextIsAtExp(tokens)) {
        val (inner, rest) = parseAtExp(tokens)
        recur(rest, APP(absyn, inner))
      } else {
        absyn -> tokens
      }
    }

    val (atExp, rest) = parseAtExp(tokens)
    recur(rest, atExp)
  }

  private def nextIsAtExp(tokens: List[Token]): Boolean = {
    tokens match {
      case Token.INT(_) :: _ => true
      case Token.FALSE :: _ => true
      case Token.TRUE :: _ => true
      case Token.VAR(_) :: _ => true
      case Token.LET :: _ => true
      case Token.LPAREN :: _ => true
      case _ => false
    }
  }

  private def parseInfExp(tokens: List[Token]): (Term, List[Token]) = {
    @annotation.tailrec
    def recur(tokens: List[Token], absyn: Term): (Term, List[Token]) = {
      // 1 token lookahead
      tokens match {
        case Token.ADD :: tokens =>
          val (inner, rest) = parseInfExp(tokens)
          recur(rest, APP(APP(VAR("+"), absyn), inner))
        case Token.SUB :: tokens =>
          val (inner, rest) = parseInfExp(tokens)
          recur(rest, APP(APP(VAR("-"), absyn), inner))
        case _ => absyn -> tokens
      }
    }

    val (atExp, rest) = parseAppExp(tokens)
    recur(rest, atExp)
  }

  // <EXP>
  private def parseExp(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case tokens if nextIsAtExp(tokens) => parseInfExp(tokens)
      case Token.IF :: tokens => parseIf(tokens)
      case Token.FN :: tokens => parseFn(tokens)
      case token :: _ => throw ParseError(token, "INT, TRUE, FALSE, VAR, LET, LPAREN, IF, FN")
      case _ => throw UnexpectedEndOfStream("INT, TRUE, FALSE, VAR, LET, LPAREN, IF, FN")
    }
  }

  private def parseIf(tokens: List[Token]): (Term, List[Token]) = {
    val (test, rest) = parseExp(tokens)
    rest match {
      case Token.THEN :: tokens =>
        val (yes, rest) = parseExp(tokens)
        rest match {
          case Token.ELSE :: tokens =>
            val (no, rest) = parseExp(tokens)
            IF(test, yes, no) -> rest
          case token :: _ => throw ParseError(token, "ELSE")
          case _ => throw UnexpectedEndOfStream("ELSE")
        }
      case token :: _ => throw ParseError(token, "THEN")
      case _ => throw UnexpectedEndOfStream("THEN")
    }
  }

  private def parseFn(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case Token.VAR(param) :: rest =>
        rest match {
          case Token.DARROW :: tokens =>
            val (body, rest) = parseExp(tokens)
            FUN(param, body) -> rest
          case token :: _ => throw ParseError(token, "DARROW")
          case _ => throw UnexpectedEndOfStream("DARROW")
        }
      case token :: _ => throw ParseError(token, "VAR")
      case _ => throw UnexpectedEndOfStream("VAR")
    }
  }
}
