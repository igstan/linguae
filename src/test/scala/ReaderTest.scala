package leesp
package test

class ReaderTest extends FunSuite with Matchers {
  val reader = new Reader(traceExecution = false)

  test("reads single-character atoms") {
    reader.read("1") should be(List(ATOM("1")))
    reader.read("a") should be(List(ATOM("a")))
    reader.read("+") should be(List(ATOM("+")))
  }

  test("reads multi-character atoms") {
    reader.read("foo") should be(List(ATOM("foo")))
    reader.read("foo-bar") should be(List(ATOM("foo-bar")))
    reader.read("bar^tax") should be(List(ATOM("bar^tax")))
  }

  test("reads multiple atoms") {
    reader.read("foo 2") should be(List(
      ATOM("foo"),
      ATOM("2")
    ))
  }

  test("reads empty list") {
    reader.read("()") should be(List(
      LIST(List())
    ))
  }

  test("reads list") {
    reader.read("(foo 2)") should be(List(
      LIST(List(
        ATOM("foo"),
        ATOM("2")
      ))
    ))
  }

  test("reads nested lists") {
    reader.read("(+ 2 (* 3 4))") should be(List(
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

  test("reads more definitions") {
    reader.read("""
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

  test("throws on unmatched left parenthesis") {
    val e = intercept[UnmatchedLeftParen] {
      reader.read("(+ 2 (* 3 4")
    }

    e.row should be(1)
    e.col should be(6)
  }

  test("throws on unmatched right parenthesis") {
    val e = intercept[UnmatchedRightParen] {
      reader.read("(+ 2 (* 3 4)\n))")
    }

    e.row should be(2)
    e.col should be(2)
  }
}
