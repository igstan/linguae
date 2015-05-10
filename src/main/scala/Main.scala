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
        let
          val id = fn a => a
        in
          if id true
          then (const 1 true) + 2
          else id 2
        end
      end
    """

    val annotatedAST = HtmlRenderer.render(Parser.parse(source))
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

    var highlighted = document.getElementById(result.id)
    highlighted.classList.add("highlight")

    termElem.addEventListener("mouseover", { (event: dom.MouseEvent) =>
      event.target match {
        case span: dom.html.Span =>
          Option(span.getAttribute("data-for-id")).filter(_.trim.nonEmpty).foreach { id =>
            document.getElementsByClassName(id).foreach(_.asInstanceOf[dom.Element].classList.add("reference"))
          }
        case other => ()
      }
    })

    termElem.addEventListener("mouseout", { (event: dom.MouseEvent) =>
      event.target match {
        case span: dom.html.Span =>
          Option(span.getAttribute("data-for-id")).filter(_.trim.nonEmpty).foreach { id =>
            document.getElementsByClassName(id).foreach(_.asInstanceOf[dom.Element].classList.remove("reference"))
          }
        case other => ()
      }
    })

    next.addEventListener("click", (event: dom.MouseEvent) =>
      result.next() match {
        case Resumption.Done(v) =>
          next.setAttribute("disabled", "true")
          println(s"Finished: $v")
          display.innerHTML = ""
          envElem.innerHTML = ""
          highlighted.classList.remove("highlight")
          display.appendChild(document.createTextNode(s"value: $v"))
        case Resumption.Next(r) =>
          result = r
          highlighted.classList.remove("highlight")
          highlighted = document.getElementById(result.id)
          highlighted.classList.add("highlight")
          envElem.innerHTML = ""
          envElem.appendChild(document.createTextNode(s"env: ${result.env}"))
      }
    )
  }
}
