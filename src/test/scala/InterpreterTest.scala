package toy
package test

class InterpreterTest extends FunSuite with Matchers {
  val env = Environment.empty
  val store = Store.empty

  test("evaluates numbers") {
    val ast = Num(1)
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(1))
  }

  test("evaluates addition expressions") {
    val ast = Add(Num(1), Add(Num(2), Num(3)))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(6))
  }

  test("evaluates subtraction expressions") {
    val ast = Sub(Num(5), Add(Num(2), Num(1)))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(2))
  }

  test("evaluates multiplication expressions") {
    val ast = Mul(Num(2), Num(5))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(10))
  }

  test("evaluates division expressions") {
    val ast = Div(Num(10), Num(5))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(2))
  }

  test("evaluates if expressions: false branch") {
    val ast = If(Num(0), Num(1), Num(2))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(2))
  }

  test("evaluates if expressions: true branch") {
    val ast = If(Num(42), Num(1), Num(2))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(1))
  }

  test("evaluates function expressions") {
    val ast = Fun("p", Num(1))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Fun("p", Num(1), env))
  }

  test("rejects non-numbers as left operands") {
    val ast = Add(Fun("p", Num(1)), Num(1))
    Interpreter.eval(ast, env, store) should be {
      Result.Failure("Left operand is not a number")
    }
  }

  test("rejects non-numbers as right operands") {
    val ast = Add(Num(1), Fun("p", Num(1)))
    Interpreter.eval(ast, env, store) should be {
      Result.Failure("Right operand is not a number")
    }
  }

  test("rejects non-numbers in condition position in if expressions") {
    val ast = If(Fun("p", Num(1)), Num(1), Num(2))
    Interpreter.eval(ast, env, store) should be {
      Result.Failure("If condition was not a number, but a function.")
    }
  }

  test("evaluates function application") {
    val ast = App(Fun("p", Num(1)), Num(2))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(1))
  }

  test("rejects non-functions in function application position") {
    val ast = App(Num(1), Num(2))
    Interpreter.eval(ast, env, store) should be {
      Result.Failure("Number in function application position.")
    }
  }

  test("evaluates parameter references") {
    val ast = App(Fun("p", Ref("p")), Num(1))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(1))
  }

  test("rejects unbound identifiers") {
    val ast = App(Fun("p", Ref("a")), Num(1))
    Interpreter.eval(ast, env, store) should be {
      Result.Failure("Unbound identifier: a")
    }
  }

  test("evaluates let expressions") {
    val ast = Let("a", Num(1), Add(Ref("a"), Num(2)))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(3))
  }

  test("supports closures") {
    // (
    //   function (makeAdder) {
    //     return (function (add10) {
    //       return add10(2);
    //     })(makeAdder(10));
    //   }
    // )(
    //   function (base) {
    //     return function (n) {
    //       return base + n;
    //     }
    //   }
    // );
    val ast = App(
      Fun("makeAdder",
        App(
          Fun("add10", App(Ref("add10"), Num(2))),
          App(Ref("makeAdder"), Num(10))
        )),
      Fun("base", Fun("n", Add(Ref("base"), Ref("n"))))
    )
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(12))
  }

  test("functions introduce static, or lexical scope, not dynamic scope") {
    // function f1(y) {
    //   return x + y;
    // }
    //
    // function f2(x) {
    //   return f1(4);
    // }
    //
    // f2(1);
    val ast = Let(
      "f1", Fun("y", Add(Ref("x"), Ref("y"))),
      Let(
        "f2", Fun("x", App(Ref("f1"), Num(4))),
        App(Ref("f2"), Num(1))
      )
    )

    Interpreter.eval(ast, env, store) should not(evaluateTo(Value.Num(5)))
    Interpreter.eval(ast, env, store) should be(Result.Failure("Unbound identifier: x"))
  }

  test("evaluates two expressions in sequence") {
    val ast = Seq(Num(1), Num(2))
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(2))
  }

  test("evaluates identifier update expressions") {
    val ast = Let(
      "a", Num(1),
      Seq(
        Set("a", Num(2)),
        Ref("a")
      )
    )
    Interpreter.eval(ast, env, store) should evaluateTo(Value.Num(2))
  }

  test("ensure scope does not leak") {
    val ast = Seq(
      Let("b", Num(3), Ref("b")),
      Ref("b")
    )

    Interpreter.eval(ast, env, store) should not(evaluateTo(Value.Num(3)))
    Interpreter.eval(ast, env, store) should be(Result.Failure("Unbound identifier: b"))
  }
}
