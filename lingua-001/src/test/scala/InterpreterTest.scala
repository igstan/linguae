package lingua001
package test

import nodes._, visitors._

class InterpreterTest extends FunSuite with Matchers {
  val log = logger("InterpreterTest")
  val initialState = Interpreter.Environment.empty[(String, Int)] -> Interpreter.Store.empty[Int, Value]

  test("evaluates a number") {
    val tree = Num(42)
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(42))
    }
  }

  test("evaluates booleans") {
    Bool(true).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Bool(true))
    }

    Bool(false).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Bool(false))
    }
  }

  test("evaluates addition") {
    val tree = Add(Num(1), Num(2))
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(3))
    }
  }

  test("evaluates variable body") {
    val tree = Let("a", Num(1)) {
      Add(Num(1), Num(2))
    }
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(3))
    }
  }

  test("evaluates variable definition") {
    val tree = Let("a", Add(Num(1), Num(2))) {
      Ref("a")
    }
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(3))
    }
  }

  test("evaluates variable reference") {
    val tree = Let("a", Num(42)) {
      Ref("a")
    }
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(42))
    }
  }

  test("evaluates function expression") {
    val env = initialState._1
    val tree = Fun("a", Ref("a"))
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Fun("a", Ref("a"), env))
    }
  }

  test("evaluates function application") {
    val tree = App(Fun("a", Add(Ref("a"), Num(2))), Num(1))
    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(3))
    }
  }

  test("evaluation supports closures") {
    // The tree below is equivalent to the following Scala code:
    //
    //   def makeAdder(base: Int) = (n: Int) => base + n
    //   val add10 = makeAdder(10)
    //   add10(2) // 12
    //
    val tree = App(
      Fun("makeAdder",
        App(
          Fun("add10", App(Ref("add10"), Num(2))), // 12
          App(Ref("makeAdder"), Num(10))
        )),
      Fun("base", Fun("n", Add(Ref("base"), Ref("n")))))

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(12))
    }
  }

  test("evaluation uses lexical scope, not dynamic scope") {
    val tree = App(
      Fun("double",
        App(
          Fun("b", App(Ref("double"), Num(2))),
          Num(3)
        )),
      Fun("a", Add(Ref("b"), Ref("b")))
    )

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Failure("unbound identifier: 'b'")
    }
  }

  test("evaluates sequences of expressions") {
    val tree = Seq(Num(1), Num(2))

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(2))
    }
  }

  test("evaluates set expression") {
    val tree = Let("a", Num(1)) {
      Seq(
        Set("a", Num(2)),
        Add(Num(0), Ref("a"))
      )
    }

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(2))
    }
  }

  test("evaluates a mutable closure") {
    val counter = Fun("n", Fun("_", Seq(Set("n", Add(Ref("n"), Num(1))), Ref("n"))))
    val tree = Let("counter", counter) {
      Let("c", App(Ref("counter"), Num(0))) {
        Seq(
          App(Ref("c"), Num(-1)),
          Seq(
            App(Ref("c"), Num(-1)),
            App(Ref("c"), Num(-1))
          )
        )
      }
    }

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(3))
    }
  }

  test("evaluates equality expressions") {
    Equal(Num(1), Num(1)).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Bool(true))
    }

    Equal(Num(1), Num(2)).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Bool(false))
    }
  }

  test("evaluates when expressions") {
    When(Bool(true), Num(1), Num(2)).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(1))
    }

    When(Bool(false), Num(1), Num(2)).accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(2))
    }
  }

  test("evaluates factorial") {
    val Y = Fun("h", App(
                   Fun("f",
                     App(Ref("f"), Ref("f"))),
                   Fun("f",
                     App(Ref("h"),
                       Fun("n",
                         App(App(Ref("f"), Ref("f")), Ref("n")))))))
    val factorial = App(Ref("Y"), Fun("fact",
            Fun("n",
              When(Equal(Ref("n"), Num(0)),
                 Num(1),
                 Mul(Ref("n"), App(Ref("fact"), Sub(Ref("n"), Num(1))))))))

    val tree = Let("Y", Y) {
      Let("factorial", factorial) {
        App(Ref("factorial"), Num(5)) // 120
      }
    }

    tree.accept(Interpreter.Visitor)(initialState)._1 should be {
      Success(Value.Num(120))
    }
  }
}
