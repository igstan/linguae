package codecamp
package test

import codecamp.Type._
import codecamp.parser.Parser

class InferTest extends Test {
  test("infer identity function") {
    val ty = Infer.typeOf(Parser.parse("""
      fn a => a
    """))

    inside (ty) { case FUN(t1, t2) => t1 should be(t2) }
  }

  test("infer const function") {
    val ty = Infer.typeOf(Parser.parse("""
      fn a => fn b => a
    """))

    inside (ty) {
      case FUN(t1, FUN(t2, t3)) =>
        t1 should be(t3)
        t1 should not be(t2)
    }
  }

  test("infer compose function") {
    val ty = Infer.typeOf(Parser.parse("""
      fn f => fn g => fn x => f (g x)
    """))

    inside (ty) {
      case FUN(FUN(t1,t2), FUN(FUN(t3,t4), FUN(t5,t6))) =>
        t5 should be(t3)
        t4 should be(t1)
        t2 should be(t6)
    }
  }

  test("infer pred function") {
    val ty = Infer.typeOf(Parser.parse("""
      fn pred => if pred 1 then 2 else 3
    """))

    ty should be(FUN(FUN(INT, BOOL), INT))
  }

  test("infer inc function") {
    val ty = Infer.typeOf(Parser.parse("""
      let
        val inc = fn a => a + 1
      in
        let
          val dec = fn a => a - 1
        in
          dec (inc 42)
        end
      end
    """))

    ty should be(INT)
  }

  test("throws unification failure") {
    val e = intercept[RuntimeException] {
      Infer.typeOf(Parser.parse("""
        let
          val const = fn y =>
            let
              val f = fn x => y
            in
              f
            end
        in
          (const true 1) + 2
        end
      """))
    }

    e.getMessage should include("cannot unify")
    e.getMessage should include("BOOL")
    e.getMessage should include("INT")
  }
}
