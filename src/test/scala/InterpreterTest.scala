package toy
package test

class InterpreterTest extends FunSuite with Matchers {
  test("evaluates numbers") {
    val ast = Num(1)
    Interpreter.eval(ast) should be(Value.Num(1))
  }
}
