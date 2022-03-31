package linguae

import scala.collection.mutable

final case class ImperativeShuntingYardParser(operatorTable: Map[String, Fixity])
  extends ExprParser {

  def parse(tokens: List[Token]): Expr = {
    val toknStack = mutable.Stack.from(tokens)
    val exprStack = mutable.Stack.empty[Expr]
    val operStack = mutable.Stack.empty[String]

    def applyOperator() = {
      val op = operStack.pop()
      val a1 = exprStack.pop()
      val a0 = exprStack.pop()
      exprStack.push(Expr.BinOp(op, a0, a1))
    }

    while (toknStack.nonEmpty) {
      toknStack.pop() match {
        case Token.Const(const) => exprStack.push(Expr.Const(const))
        case Token.ParenL => operStack.push("(")

        case Token.ParenR =>
          while (operStack.nonEmpty && operStack.top != "(") {
            applyOperator()
          }

          if (operStack.top == "(") operStack.pop()
          else sys.error("missing open parenthesis")

        case Token.BinOp(binOp) =>
          val binOpFixity = operatorTable(binOp)
          var pendingOperators = true

          while (pendingOperators) {
            if (operStack.isEmpty || operStack.top == "(") {
              pendingOperators = false
            } else {
              val topOpFixity = operatorTable(operStack.top)

              if (binOpFixity.appliesAfter(topOpFixity)) {
                applyOperator()
              } else {
                pendingOperators = false
              }
            }
          }

          operStack.push(binOp)
      }
    }

    while (operStack.nonEmpty) {
      applyOperator()
    }

    exprStack
      .pop()
      .ensuring(exprStack.isEmpty, s"non-empty expression stack: $exprStack")
  }
}
