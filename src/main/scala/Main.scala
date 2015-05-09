package ro.igstan.debugger

import scala.scalajs.js.JSApp

import org.scalajs.dom, dom.document

import display._
import eval._
import syntax._, Term._

object Main extends JSApp {
  def main(): Unit = {
    val source = """
      let
        val const = fn y =>
          let
            val f = fn x => y
          in
            f
          end
      in
        (const 1 true) + 2
      end
    """

    // val term = APP(FN("x", APP(APP(VAR("+"), VAR("x")), VAR("x"))), INT(2))
    val term = Parser.parse(source)
    val annotatedAST = HtmlRenderer.render(term)
    var result = Interpreter.eval(annotatedAST, Env.empty)(identity)


    document.body.innerHTML = """
      <button id="step-in">step in</button>
      <button id="reset">reset</button>
      <div id="result"></div>
      <div id="env"></div>
      <pre id="term"></pre>
    """

    val next = document.getElementById("step-in")
    val reset = document.getElementById("reset")
    val display = document.getElementById("result")
    val envElem = document.getElementById("env")
    val termElem = document.getElementById("term")

    termElem.innerHTML = annotatedAST.meta

    reset.addEventListener("click", (event: dom.MouseEvent) => main())

    next.addEventListener("click", (event: dom.MouseEvent) =>
      result.next() match {
        case Resumption.Done(v) =>
          next.setAttribute("disabled", "true")
          println(s"Finished: $v")
          display.innerHTML = ""
          display.appendChild(document.createTextNode(s"value: $v"))
        case Resumption.Next(r) =>
          val env = Env(r.env.bindings.filter {
            case (_, _: Value.Native) => false
            case _ => true
          })

          envElem.innerHTML = ""
          envElem.appendChild(document.createTextNode(s"env: $env"))
          result = r
      }
    )
  }
}
