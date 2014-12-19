package toy
package test

class InterpreterTest extends FunSuite with Matchers {
  test("evaluates numbers") {
    val ast = Num(1)
    Interpreter.eval(ast) should be(Value.Num(1))
  }

  test("evaluates addition expressions") {
    val ast = Add(Num(1), Add(Num(2), Num(3)))
    Interpreter.eval(ast) should be(Value.Num(6))
  }

  test("evaluates subtraction expressions") {
    val ast = Sub(Num(5), Add(Num(2), Num(1)))
    Interpreter.eval(ast) should be(Value.Num(2))
  }

  test("evaluates multiplication expressions") {
    val ast = Mul(Num(2), Num(5))
    Interpreter.eval(ast) should be(Value.Num(10))
  }

  test("evaluates division expressions") {
    val ast = Div(Num(10), Num(5))
    Interpreter.eval(ast) should be(Value.Num(2))
  }
}
