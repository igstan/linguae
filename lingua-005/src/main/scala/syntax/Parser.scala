package ro.igstan.debugger
package syntax

case class UnexpectedEndOfStream(expected: Token*)
  extends RuntimeException("Unexpected end of stream; expected %s".format(expected.mkString(", ")))

case class ParseError(found: Token, expected: Token*)
  extends RuntimeException("Unexpected token: %s; expected %s".format(found, expected.mkString(", ")))

object Parser {
  import Token._

  def parse(source: String): Term = {
    parse(Scanner.scan(source))
  }

  def parse(tokens: List[Token]): Term = {
    parseExp(tokens) match {
      case (term, Nil) => term
      case (_, token :: _) =>
        throw ParseError(token, INT(0), TRUE, FALSE, VAR("?"), IF, FN, LET)
    }
  }

  // <ATEXP>
  private def parseAtExp(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case INT(n) :: tokens => Term.INT(n)("", "") -> tokens
      case TRUE :: tokens => Term.BOOL(true)("", "") -> tokens
      case FALSE :: tokens => Term.BOOL(false)("", "") -> tokens
      case VAR(id) :: tokens => Term.VAR(id)("", "") -> tokens
      case LET :: tokens => parseLet(tokens)
      case LPAREN :: tokens =>
        val (exp, rest) = parseExp(tokens)
        rest match {
          case RPAREN :: tokens => exp -> tokens
          case token :: _ => throw ParseError(token, RPAREN)
          case _ => throw UnexpectedEndOfStream(RPAREN)
        }
      case token :: _ => throw ParseError(token, INT(0), TRUE, FALSE, VAR("?"), LET, LPAREN)
      case _ => throw UnexpectedEndOfStream(INT(0), TRUE, FALSE, VAR("?"), LET, LPAREN)
    }
  }

  private def parseLet(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case VAL :: tokens =>
        tokens match {
          case VAR(bindingName) :: tokens =>
            tokens match {
              case EQUAL :: tokens =>
                val (bindingValue, rest) = parseExp(tokens)
                rest match {
                  case IN :: tokens =>
                    val (body, rest) = parseExp(tokens)
                    rest match {
                      case END :: tokens =>
                        Term.LET(bindingName, bindingValue, body)("", "") -> tokens
                      case token :: _ => throw ParseError(token, END)
                      case _ => throw UnexpectedEndOfStream(END)
                    }
                  case token :: _ => throw ParseError(token, IN)
                  case _ => throw UnexpectedEndOfStream(IN)
                }
              case token :: _ => throw ParseError(token, EQUAL)
              case _ => throw UnexpectedEndOfStream(EQUAL)
            }
          case token :: _ => throw ParseError(token, VAR("?"))
          case _ => throw UnexpectedEndOfStream(VAR("?"))
        }
      case token :: _ => throw ParseError(token, VAL)
      case _ => throw UnexpectedEndOfStream(VAL)
    }
  }

  // <APPEXP>
  private def parseAppExp(tokens: List[Token]): (Term, List[Token]) = {
    @annotation.tailrec
    def recur(tokens: List[Token], term: Term): (Term, List[Token]) = {
      // 1 token lookahead
      if (nextIsAtExp(tokens)) {
        val (inner, rest) = parseAtExp(tokens)
        recur(rest, Term.APP(term, inner)("", ""))
      } else {
        term -> tokens
      }
    }

    val (atExp, rest) = parseAtExp(tokens)
    recur(rest, atExp)
  }

  private def nextIsAtExp(tokens: List[Token]): Boolean = {
    tokens match {
      case INT(_) :: _ => true
      case FALSE :: _ => true
      case TRUE :: _ => true
      case VAR(_) :: _ => true
      case LET :: _ => true
      case LPAREN :: _ => true
      case _ => false
    }
  }

  private def parseInfExp(tokens: List[Token]): (Term, List[Token]) = {
    @annotation.tailrec
    def recur(tokens: List[Token], term: Term): (Term, List[Token]) = {
      // 1 token lookahead
      tokens match {
        case ADD :: tokens =>
          val (inner, rest) = parseInfExp(tokens)
          recur(rest, Term.ADD(term, inner)("", ""))
        case SUB :: tokens =>
          val (inner, rest) = parseInfExp(tokens)
          recur(rest, Term.SUB(term, inner)("", ""))
        case _ => term -> tokens
      }
    }

    val (atExp, rest) = parseAppExp(tokens)
    recur(rest, atExp)
  }

  // <EXP>
  private def parseExp(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case tokens if nextIsAtExp(tokens) => parseInfExp(tokens)
      case IF :: tokens => parseIf(tokens)
      case FN :: tokens => parseFn(tokens)
      case token :: _ => throw ParseError(token, INT(0), TRUE, FALSE, VAR("?"), LET, LPAREN, IF, FN)
      case _ => throw UnexpectedEndOfStream(INT(0), TRUE, FALSE, VAR("?"), LET, LPAREN, IF, FN)
    }
  }

  private def parseIf(tokens: List[Token]): (Term, List[Token]) = {
    val (test, rest) = parseExp(tokens)
    rest match {
      case THEN :: tokens =>
        val (yes, rest) = parseExp(tokens)
        rest match {
          case ELSE :: tokens =>
            val (no, rest) = parseExp(tokens)
            Term.IF(test, yes, no)("", "") -> rest
          case token :: _ => throw ParseError(token, ELSE)
          case _ => throw UnexpectedEndOfStream(ELSE)
        }
      case token :: _ => throw ParseError(token, THEN)
      case _ => throw UnexpectedEndOfStream(THEN)
    }
  }

  private def parseFn(tokens: List[Token]): (Term, List[Token]) = {
    tokens match {
      case VAR(param) :: rest =>
        rest match {
          case DARROW :: tokens =>
            val (body, rest) = parseExp(tokens)
            Term.FN(param, body)("", "") -> rest
          case token :: _ => throw ParseError(token, DARROW)
          case _ => throw UnexpectedEndOfStream(DARROW)
        }
      case token :: _ => throw ParseError(token, VAR("?"))
      case _ => throw UnexpectedEndOfStream(VAR("?"))
    }
  }
}
