package codecamp
package test
package parser

import codecamp.parser.{ Scanner, Token }

class ScannerTest extends Test {
  test("scans identity") {
    val tokens = Scanner.scan("""
      fn a => a
    """)

    tokens should be(List(
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.VAR("a")
    ))
  }

  test("scans const") {
    val tokens = Scanner.scan("""
      fn a => fn b => a
    """)

    tokens should be(List(
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.FN,
      Token.VAR("b"),
      Token.DARROW,
      Token.VAR("a")
    ))
  }

  test("scans compose") {
    val tokens = Scanner.scan("""
      fn f => fn g => fn x => f (g x)
    """)

    tokens should be(List(
      Token.FN,
      Token.VAR("f"),
      Token.DARROW,
      Token.FN,
      Token.VAR("g"),
      Token.DARROW,
      Token.FN,
      Token.VAR("x"),
      Token.DARROW,
      Token.VAR("f"),
      Token.LPAREN,
      Token.VAR("g"),
      Token.VAR("x"),
      Token.RPAREN
    ))
  }

  test("scans pred") {
    val tokens = Scanner.scan("""
      fn pred => if pred 1 then 2 else 3
    """)

    tokens should be(List(
      Token.FN,
      Token.VAR("pred"),
      Token.DARROW,
      Token.IF,
      Token.VAR("pred"),
      Token.INT(1),
      Token.THEN,
      Token.INT(2),
      Token.ELSE,
      Token.INT(3)
    ))
  }

  test("scans inc") {
    val tokens = Scanner.scan("""
      let
        val inc = fn a => a + 1
      in
        let
          val dec = fn a => a - 1
        in
          dec (inc 42)
        end
      end
    """)

    tokens should be(List(
      Token.LET,
      Token.VAL,
      Token.VAR("inc"),
      Token.EQUAL,
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.VAR("a"),
      Token.ADD,
      Token.INT(1),
      Token.IN,
      Token.LET,
      Token.VAL,
      Token.VAR("dec"),
      Token.EQUAL,
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.VAR("a"),
      Token.SUB,
      Token.INT(1),
      Token.IN,
      Token.VAR("dec"),
      Token.LPAREN,
      Token.VAR("inc"),
      Token.INT(42),
      Token.RPAREN,
      Token.END,
      Token.END
    ))
  }
}
