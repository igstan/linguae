package codecamp
package test
package parser

import codecamp.parser.{ Parser, Token }

class ParserTest extends Test {
  test("parses identity") {
    val ast = Parser.parse(List(
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.VAR("a")
    ))

    ast should be {
      FUN("a", VAR("a"))
    }
  }

  test("parses const") {
    val ast = Parser.parse(List(
      Token.FN,
      Token.VAR("a"),
      Token.DARROW,
      Token.FN,
      Token.VAR("b"),
      Token.DARROW,
      Token.VAR("a")
    ))

    ast should be {
      FUN("a", FUN("b", VAR("a")))
    }
  }

  test("parses compose") {
    val ast = Parser.parse(List(
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

    ast should be {
      FUN("f", FUN("g", FUN("x", APP(VAR("f"), APP(VAR("g"), VAR("x"))))))
    }
  }

  test("parses pred") {
    val ast = Parser.parse(List(
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

    ast should be {
      FUN(
        "pred",
        IF(
          APP(VAR("pred"), INT(1)),
          INT(2),
          INT(3)
        )
      )
    }
  }

  test("parses inc") {
    val ast = Parser.parse(List(
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

    ast should be {
      LET(
        "inc", FUN("a", APP(APP(VAR("+"), VAR("a")), INT(1))),
        LET(
          "dec", FUN("a", APP(APP(VAR("-"), VAR("a")), INT(1))),
          APP(VAR("dec"), APP(VAR("inc"), INT(42)))
        )
      )
    }
  }
}
