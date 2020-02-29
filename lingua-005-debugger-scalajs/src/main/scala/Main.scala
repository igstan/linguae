package ro.igstan.debugger

import org.scalajs.dom
import org.scalajs.dom.{ document, html }
import ro.igstan.debugger.display.HtmlRenderer
import ro.igstan.debugger.eval.{ Env, Interpreter, Result, Resumption, Value }
import ro.igstan.debugger.syntax.Parser

object Main {
  def main(args: Array[String]): Unit = {
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
    var result = Resumption(Env.empty, annotatedAST.id, None) { () =>
      Resumption.Next(Interpreter.eval(annotatedAST, Env.empty)(identity))
    }

    document.body.innerHTML = """
      <button id="step-in">step in</button>
      <button id="reset">reset</button>
      <pre id="term"></pre>
      <div>last: <span id="last-result"></span></div>
      <div id="result"></div>
      <div id="env"></div>
      <canvas id="overlay" class="overlay"></canvas>
    """

    val next = document.getElementById("step-in")
    val reset = document.getElementById("reset")
    val display = document.getElementById("result")
    val envElem = document.getElementById("env")
    val termElem = document.getElementById("term")
    val lastResult = document.getElementById("last-result")
    val overlay = document.getElementById("overlay").asInstanceOf[html.Canvas]

    overlay.width = document.documentElement.clientWidth - 40   // body margins
    overlay.height = document.documentElement.clientHeight - 40 // body margins

    val renderer = overlay.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderer.strokeStyle = "#FFC9D7"

    termElem.innerHTML = annotatedAST.meta
    reset.addEventListener("click", (_: dom.MouseEvent) => main(Array.empty))

    var highlighted = document.getElementById(result.id)

    termElem.addEventListener("mouseover", { (event: dom.MouseEvent) =>
      event.target match {
        case span: dom.html.Span =>
          Option(span.getAttribute("data-for-id")).filter(_.trim.nonEmpty).foreach { id =>
            val binder = document.getElementById(id)
            binder.classList.add("reference")
            val start = binder.getBoundingClientRect()

            document.getElementsByClassName(id).filter(_ != binder).foreach { e =>
              val elem = e.asInstanceOf[dom.Element]
              elem.classList.add("reference")
              val end = elem.getBoundingClientRect()
              renderer.beginPath()
              renderer.moveTo(start.left + start.width / 2 - 20, start.bottom - 20) // - 20, compenstate for body margin
              renderer.lineTo(end.left + end.width / 2 - 20, end.top - 20)          // - 20, compenstate for body margin
              renderer.closePath()
              renderer.stroke()
            }
          }
        case _ => ()
      }
    })

    termElem.addEventListener("mouseout", { event: dom.MouseEvent =>
      event.target match {
        case span: dom.html.Span =>
          renderer.clearRect(0, 0, overlay.width.toDouble, overlay.height.toDouble)
          Option(span.getAttribute("data-for-id")).filter(_.trim.nonEmpty).foreach { id =>
            document.getElementsByClassName(id).foreach(_.asInstanceOf[dom.Element].classList.remove("reference"))
          }
        case _ => ()
      }
    })

    next.addEventListener("click", (_: dom.MouseEvent) =>
      result.next() match {
        case Resumption.Done(v) =>
          next.setAttribute("disabled", "true")
          println(s"Finished: $v")
          display.innerHTML = ""
          envElem.innerHTML = ""
          highlighted.classList.remove("highlight")
          display.appendChild(document.createTextNode(s"value: ${displayResult(Some(v))}"))
        case Resumption.Next(r) =>
          result = r
          highlighted.classList.remove("highlight")
          highlighted = document.getElementById(result.id)
          highlighted.classList.add("highlight")
          lastResult.innerHTML = displayResult(r.prev)
          envElem.innerHTML = ""
          envElem.appendChild(document.createTextNode(s"env: ${result.env}"))
      }
    )
  }

  def displayResult(result: Option[Result]): String = {
    result match {
      case None => ""
      case Some(Left(error)) => s"error: $error"
      case Some(Right(value)) =>
        value match {
          case Value.Num(value) => s"$value"
          case Value.Bool(value) => s"$value"
          case Value.Fun(_, _, _) => "ƒ"
        }
    }
  }
}
