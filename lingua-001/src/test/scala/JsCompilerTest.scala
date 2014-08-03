package lingua001
package test

import nodes._, visitors._

class JsCompilerTest extends FunSuite with Matchers {
  val log = logger("InterpreterTest")

  test("compiles function reference application") {
    val tree = App(Ref("someFunction"), Num(1))
    val js = tree.accept(JsCompiler)(Trace.empty)._1
    js should be("someFunction(1)")
  }

  test("compiles function literal application") {
    val tree = App(Fun("a", Ref("a")), Num(1))
    val js = tree.accept(JsCompiler)(Trace.empty)._1
    js should be("(function(a){return a})(1)")
  }

  test("compiles return statement to correct position") {
    val tree = Seq(Num(1), Seq(Num(2), Num(3)))
    val js = tree.accept(JsCompiler)(Trace(returning = true))._1
    js should be("1;2;return 3")
  }

  test("compiles let bindings to JS vars") {
    val tree = Let("a", Num(1)) { Ref("a") }
    val js = tree.accept(JsCompiler)(Trace(returning = true))._1
    js should be("var a=1;return a")
  }

  ignore("compiles shadowing let bindings to IIFEs") {
    val tree = Let("a", Num(1)) {
      Let("a", Num(2)) {
        Ref("a")
      }
    }
    val js = tree.accept(JsCompiler)(Trace(returning = true))._1
    js should be("var a=1;return (function(a){return a})(2)")
  }
}
