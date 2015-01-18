package leesp
package test

class ParserTest extends FunSuite with Matchers {
  val parser = new Parser(traceExecution = false)

  test("parses single-character atoms") {
    parser.parse("1") should be(List(ATOM("1")))
    parser.parse("a") should be(List(ATOM("a")))
    parser.parse("+") should be(List(ATOM("+")))
  }

  test("parses multi-character atoms") {
    parser.parse("foo") should be(List(ATOM("foo")))
    parser.parse("foo-bar") should be(List(ATOM("foo-bar")))
    parser.parse("bar^tax") should be(List(ATOM("bar^tax")))
  }

  test("parses multiple atoms") {
    parser.parse("foo 2") should be(List(
      ATOM("foo"),
      ATOM("2")
    ))
  }

  test("parses empty list") {
    parser.parse("()") should be(List(
      LIST(List())
    ))
  }

  test("parses list") {
    parser.parse("(foo 2)") should be(List(
      LIST(List(
        ATOM("foo"),
        ATOM("2")
      ))
    ))
  }

  test("parses nested lists") {
    parser.parse("(+ 2 (* 3 4))") should be(List(
      LIST(List(
        ATOM("+"),
        ATOM("2"),
        LIST(List(
          ATOM("*"),
          ATOM("3"),
          ATOM("4")
        ))
      ))
    ))
  }

  test("throws on unmatched left parenthesis") {
    val e = intercept[UnmatchedLeftParen] {
      parser.parse("(+ 2 (* 3 4")
    }

    e.row should be(1)
    e.col should be(6)
  }

  test("throws on unmatched right parenthesis") {
    val e = intercept[UnmatchedRightParen] {
      parser.parse("(+ 2 (* 3 4)\n))")
    }

    e.row should be(2)
    e.col should be(2)
  }

  test("parses more definitions") {
    parser.parse("""
      (define a 1)
      (define b 2)
      (define add (lambda (a b) (+ a b)))
      (add a b)
    """) should be(List(
      LIST(List(ATOM("define"), ATOM("a"), ATOM("1"))),
      LIST(List(ATOM("define"), ATOM("b"), ATOM("2"))),
      LIST(List(
        ATOM("define"),
        ATOM("add"),
        LIST(List(
          ATOM("lambda"),
          LIST(List(ATOM("a"), ATOM("b"))),
          LIST(List(ATOM("+"), ATOM("a"), ATOM("b")))
        ))
      )),
      LIST(List(ATOM("add"), ATOM("a"), ATOM("b")))
    ))
  }
}
