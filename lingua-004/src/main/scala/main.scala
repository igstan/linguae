package leesp

object Main {
  val reader = new Reader(traceExecution = false)

  def main(args: Array[String]): Unit = {
    showProgram(args(0))
    showProgram("""
      (define a 1)
      (define b 2)
      (define add (lambda (a b) (+ a b)))
      (add a b)
    """)
  }

  def showProgram(source: String): Unit = {
    try {
      println(reader.read(source).mkString("\n"))
    } catch {
      case UnmatchedLeftParen(row, col) =>
        println(s"unmatched left parenthesis: row=$row, col=$col")
      case UnmatchedRightParen(row, col) =>
        println(s"unmatched right parenthesis: row=$row, col=$col")
    }
  }
}
