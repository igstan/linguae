package lingua001

import nodes._
import visitors._

object Main {
  val log = logger("Main")

  def main(args: Array[String]): Unit = {
    val initialState = Interpreter.Environment.empty[(String, Int)] -> Interpreter.Store.empty[Int, Value]
    // val tree = App(
    //   Fun("makeAdder",
    //     App(
    //       Fun("add10", App(Ref("add10"), Num(2))), // 12
    //       App(Ref("makeAdder"), Num(10))
    //     )),
    //   Fun("base", Fun("n", Add(Ref("base"), Ref("n")))))

    // val counter = Fun("n", Fun("_", Seq(Set("n", Add(Ref("n"), Num(1))), Ref("n"))))
    // val tree = Let("counter", counter) {
    //   Let("c", App(Ref("counter"), Num(0))) {
    //     Seq(
    //       App(Ref("c"), Num(-1)),
    //       Seq(
    //         App(Ref("c"), Num(-1)),
    //         App(Ref("c"), Num(-1))
    //       )
    //     )
    //   }
    // }

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

    val intrp = tree.accept(JsCompiler)(Trace.empty)._1

    println(s"Intrp: $intrp")

    // val tree = Let("a", Num(40)) {
    //   Let("b", Num(2)) {
    //     Add(Ref("a"), Ref("c"))
    //   }
    // }

    // val depth = tree.accept(Depth)(())
    // val intrp = tree.accept(Interpreter.Visitor)

    // println(s"Tree: $tree")
    // println(s"Depth: $depth")
    // println(s"Intrp: $intrp")
  }
}
