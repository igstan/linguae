package linguae

import scala.annotation.tailrec

final case class ShuntingYardParser(operatorTable: Map[String, Fixity]) {
  def parse(tokens: List[Token]): Expr = {
    def applyOp(op: String, exprs: List[Expr]): List[Expr] =
      exprs match {
        case b :: a :: restExprs =>
          Expr.BinOp(op, a, b) :: restExprs
        case _ =>
          sys.error(s"insufficient operands for operator: $op")
      }

    @tailrec
    def binOp(
      opX: String,
      tokens: List[Token],
      ops: List[String],
      exprs: List[Expr],
    ): Expr =
      ops match {
        case opY :: restOps if opY != "(" =>
          val xFixity = operatorTable(opX)
          val yFixity = operatorTable(opY)

          if (xFixity.appliesAfter(yFixity)) {
            binOp(opX, tokens, restOps, applyOp(opY, exprs))
          } else {
            loop(tokens, opX :: ops, exprs)
          }

        case _ =>
          loop(tokens, opX :: ops, exprs)
      }

    @tailrec
    def rparen(tokens: List[Token], ops: List[String], exprs: List[Expr]): Expr =
      ops match {
        case Nil => loop(tokens, Nil, exprs)
        case "(" :: restOps => loop(tokens, restOps, exprs)
        case op :: restOps => rparen(tokens, restOps, applyOp(op, exprs))
      }

    @tailrec
    def loop(tokens: List[Token], ops: List[String], exprs: List[Expr]): Expr =
      tokens match {
        case Nil =>
          ops match {
            case Nil => exprs.head
            case op :: restOps => rparen(tokens, restOps, applyOp(op, exprs))
          }

        case token :: restTokens =>
          token match {
            case Token.Const(value) => loop(restTokens, ops, Expr.Const(value) :: exprs)
            case Token.LParen => loop(restTokens, "(" :: ops, exprs)
            case Token.RParen => rparen(restTokens, ops, exprs)
            case Token.BinOp(op) => binOp(op, restTokens, ops, exprs)
          }
      }

    loop(tokens, List.empty, List.empty)
  }
}
