package toy
package test

class InterpreterTest extends FunSuite with Matchers {
  test("evaluates numbers") {
    val ast = Num(1)
    Interpreter.eval(ast) should be(Result.Success(Value.Num(1)))
  }

  test("evaluates addition expressions") {
    val ast = Add(Num(1), Add(Num(2), Num(3)))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(6)))
  }

  test("evaluates subtraction expressions") {
    val ast = Sub(Num(5), Add(Num(2), Num(1)))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(2)))
  }

  test("evaluates multiplication expressions") {
    val ast = Mul(Num(2), Num(5))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(10)))
  }

  test("evaluates division expressions") {
    val ast = Div(Num(10), Num(5))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(2)))
  }

  test("evaluates if expressions: false branch") {
    val ast = If(Num(0), Num(1), Num(2))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(2)))
  }

  test("evaluates if expressions: true branch") {
    val ast = If(Num(42), Num(1), Num(2))
    Interpreter.eval(ast) should be(Result.Success(Value.Num(1)))
  }

  test("evaluates function expressions") {
    val ast = Fun("p", Num(1))
    Interpreter.eval(ast) should be(Result.Success(Value.Fun("p", Num(1))))
  }

  test("rejects non-numbers as left operands") {
    val ast = Add(Fun("p", Num(1)), Num(1))
    Interpreter.eval(ast) should be {
      Result.Failure("Left operand is not a number")
    }
  }

  test("rejects non-numbers as right operands") {
    val ast = Add(Num(1), Fun("p", Num(1)))
    Interpreter.eval(ast) should be {
      Result.Failure("Right operand is not a number")
    }
  }

  test("rejects non-numbers in condition position in if expressions") {
    val ast = If(Fun("p", Num(1)), Num(1), Num(2))
    Interpreter.eval(ast) should be {
      Result.Failure("If condition was not a number, but a function.")
    }
  }
}
