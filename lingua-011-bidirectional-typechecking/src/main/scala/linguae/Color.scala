package linguae

object Color {
  private val reset = "\u001b[0m"

  private def xterm256(c: Int): String =
    "\u001b[1;38;5;%dm".format(c)

  def keyword(s: String): String =
    xterm256(90) + s + reset

  private def xterm256(r: Int, g: Int, b: Int): String = {
    val color = 16 + (r * 36) + (g * 6) + b
    "\u001b[48;5;%dm".format(color)
  }

  def main(args: Array[String]): Unit = {
    println(keyword("class"))

    for {
      r <- 0.until(6)
      g <- 0.until(6)
      b <- 0.until(6)
    } yield {
      print(xterm256(r, g, b) + "  " + reset)
      if (b == 5) println()
    }
  }
}
