package ro.igstan.debugger

import scala.scalajs.js.JSApp

import org.scalajs.dom, dom.document

import eval._
import syntax.Term._

object Main extends JSApp {
  def main(): Unit = {
    val term = App(Fun("x", App(App(Var("+"), Var("x")), Var("x"))), Num(2))
    var result = Interpreter.eval(term, Env.predef)(identity)

    document.body.innerHTML = """
      <button id="step-in">step in</button>
      <div id="result"></div>
      <div id="env"></div>
    """

    val next = document.getElementById("step-in")
    val display = document.getElementById("result")
    val envElem = document.getElementById("env")

    next.addEventListener("click", (event: dom.MouseEvent) =>
      result.next() match {
        case Resumption.Done(v) =>
          next.setAttribute("disabled", "true")
          println(s"finished execution: $v")
          display.innerHTML = s"value: $v"
        case Resumption.Next(r) =>
          val env = r.env.bindings.filter {
            case (_, _: Value.Native) => false
            case _ => true
          }

          envElem.innerHTML = s"env: $env"
          result = r
      }
    )
  }
}
