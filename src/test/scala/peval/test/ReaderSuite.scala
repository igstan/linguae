package peval
package test

final class ReaderSuite extends munit.FunSuite {
  test("[") {
    intercept[UnbalancedParenL](Reader.read("["))
  }

  test("]") {
    intercept[UnbalancedParenR](Reader.read("]"))
  }

  test("atom") {
    val result = Reader.read("atom")
    assertEquals(result, List(ATOM("atom")))
  }

  test("atom 1234") {
    val result = Reader.read("atom 1234")
    assertEquals(result, List(ATOM("atom"), ATOM("1234")))
  }

  test("[atom]") {
    val result = Reader.read("[atom]")
    assertEquals(result, List(LIST(ATOM("atom"))))
  }

  test("[[atom] atom]") {
    val result = Reader.read("[[atom] atom]")
    assertEquals(result, List(LIST(LIST(ATOM("atom")), ATOM("atom"))))
  }

  test("[atom [atom]]") {
    val result = Reader.read("[atom [atom]]")
    assertEquals(result, List(LIST(ATOM("atom"), LIST(ATOM("atom")))))
  }

  test("[atom [atom]] multiline") {
    val result = Reader.read("""
      [atom
        [atom]]
    """)

    assertEquals(result, List(LIST(ATOM("atom"), LIST(ATOM("atom")))))
  }

  test("allows comments") {
    val result = Reader.read("""
      ; comments are supported
      [atom
        ; here too
        [atom]
        ; as well as here
        ]
    """)

    assertEquals(result, List(LIST(ATOM("atom"), LIST(ATOM("atom")))))
  }

  test("reads from stream") {
    val result = Reader.read(getClass.getResourceAsStream("./data/test.leesp"))
    assertEquals(result, List(LIST(ATOM("atom"), LIST(ATOM("atom")))))
  }
}
