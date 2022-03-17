package peval
package test

final class NaiveSpecializerSuite extends munit.FunSuite {

  test("specializes file") {
    val program = Parser(Reader.read(getClass.getResourceAsStream("./data/power.leesp")))
    val residual = NaiveSpecializer.specialize(program)
    val expected = Parser(Reader.read("[* x [* x [* x [* x [* x x]]]]]").head)

    assertEquals(residual, expected)
  }

  test("specializes: [power x 3]") {
    val source = """
      [def power [x n]
        [if [= n 0]
            1
            [* x [power x [- n 1]]]]]

      [def main []
        [power x 3]]
    """

    val program = Parser(Reader.read(source))
    val residual = NaiveSpecializer.specialize(program)
    val expected = Parser(Reader.read("[* x [* x x]]").head)

    assertEquals(residual, expected)
  }

  test("overflows the stack: [power 3 n]") {
    val source = """
      [def power [x n]
        [if [= n 0]
            1
            [* x [power x [- n 1]]]]]

      [def main []
        [power 3 n]]
    """

    val program = Parser(Reader.read(source))

    try {
      NaiveSpecializer.specialize(program)
      fail("should have overflown the stack")
    } catch {
      case _: StackOverflowError => ()
    }
  }
}
