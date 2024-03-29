package peval

object Main {
  def main(args: Array[String]): Unit = {
    println()
    println("NAIVE SPECIALIZER")
    println("=================")
    naive()
    println()
    println("BETTER SPECIALIZER")
    println("==================")
    better()
    println()
  }

  def naive(): Unit = {
    val source = getClass.getResourceAsStream("/leesp/exp.leesp")
    val program = Parser(Reader.read(source))
    val residue = NaiveSpecializer.specialize(program)

    println()
    println("PROGRAM:")
    println(program)
    println()
    println("RESIDUE:")
    println(residue)
    println()
  }

  private def better(): Unit = {
    val source = """
      [def power [x n]
        [if [= n 0]
            1
            [* x [power x [- n 1]]]]]

      [def main []
        [power x 3]]
    """

    val program = Parser(Reader.read(source))
    val residue = BetterSpecializer.specialize(program)

    println()
    println("PROGRAM:")
    println(program)
    println()
    println("RESIDUE:")
    println(residue.program)
    println()
    println("LOGS:")
    println(residue.logs.reverse.mkString("\n\n"))
    println()
  }
}
