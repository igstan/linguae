package codecamp

import parser.Parser
import signature.Signature

object Main {
  def main(args: Array[String]): Unit = {
    val source = """
    | fn isZero =>
    |   if isZero 1
    |   then 2
    |   else 3
    """.trim.stripMargin

    println(s"\n$source\n")

    val ast = Parser.parse(source)
    println(s"Term: $ast")

    val inferredType = Infer.typeOf(ast)
    println("Type: " + Signature.forType(inferredType) + "\n")
  }
}
