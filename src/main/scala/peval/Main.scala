package peval

object Main {
  def main(args: Array[String]): Unit = {
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
}
