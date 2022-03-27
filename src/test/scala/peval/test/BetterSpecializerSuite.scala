package peval
package test

final class BetterSpecializerSuite extends munit.FunSuite {
  test("specializes: [power x 3]") {
    val source = """
      [def power [x n]
        [if [= n 0]
            1
            [* x [power x [- n 1]]]]]

      [def main []
        [power x 3]]
    """

    val expected = List(
      "[def fn-872237902 [fun [x] [* x [fn-872238863 x]]]]",
      "[def fn-872238863 [fun [x] [* x [fn-872239824 x]]]]",
      "[def fn-872239824 [fun [x] [* x [fn-872240785 x]]]]",
      "[def fn-872240785 [fun [x] 1]]",
      "[def main [] [fn-872237902 x]]",
    )

    val program = Parser(Reader.read(source))
    val residue = BetterSpecializer.specialize(program)

    assertEquals(residue.program.toString, expected.mkString("\n"))
  }

  test("specializes: [power 3 n] (no stack overflow)") {
    val source = """
      [def power [x n]
        [if [= n 0]
            1
            [* x [power x [- n 1]]]]]

      [def main []
        [power 3 n]]
    """

    val expected = List(
      "[def fn-863002692 [fun [n] [if [= n 0] 1 [* 3 [fn-863002692 [- n 1]]]]]]",
      "[def main [] [fn-863002692 n]]",
    )

    val program = Parser(Reader.read(source))
    val residue = BetterSpecializer.specialize(program)

    assertEquals(residue.program.toString, expected.mkString("\n"))
  }
}
