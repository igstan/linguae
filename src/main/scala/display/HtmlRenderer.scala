package ro.igstan.debugger
package display

import syntax.Term

object HtmlRenderer {
  import Term._

  def render(term: Term): Term = {
    var counter = 0

    def nextID(): String = {
      counter += 1
      s"id-$counter"
    }

    def wrap(term: Term, level: Int, env: Map[String, String]): Term = {
      term match {
        case APP(fn, arg) =>
          val id = nextID()
          val fnR = loop(fn, level, env)
          val argR = loop(arg, level, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="app"><span class="left-paren">(</span>""",
            s"""<span class="app-fn">${fnR.meta}</span>""", " ",
            s"""<span class="app-arg">${argR.meta}</span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          APP(fnR, argR)(html.mkString(""), id)
        case ADD(a, b) =>
          val id = nextID()
          val aR = wrap(a, 0, env)
          val bR = wrap(b, 0, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="add"><span class="left-paren">(</span>""",
            s"""<span class="left-operand">${aR.meta}</span>""", " ",
            s"""<span class="symbol add">+</span>""", " ",
            s"""<span class="right-operand">${bR.meta}</span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          ADD(aR, bR)(html.mkString(""), id)
        case SUB(a, b) =>
          val id = nextID()
          val aR = wrap(a, 0, env)
          val bR = wrap(b, 0, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="sub"><span class="left-paren">(</span>""",
            s"""<span class="left-operand">${aR.meta}</span>""", " ",
            s"""<span class="symbol sub">-</span>""", " ",
            s"""<span class="right-operand">${bR.meta}</span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          SUB(aR, bR)(html.mkString(""), id)
        case other => loop(other, level, env)
      }
    }

    def loop(term: Term, level: Int, env: Map[String, String]): Term = {
      term match {
        case INT(value) =>
          val id = nextID()
          INT(value)(indent(level) + s"""<span id="$id" class="int">$value</span>""", id)
        case ADD(a, b) =>
          val id = nextID()
          val aR = wrap(a, 0, env)
          val bR = wrap(b, 0, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="add">""",
            s"""<span class="left-operand">${aR.meta}</span>""", " ",
            s"""<span class="symbol add">+</span>""", " ",
            s"""<span class="right-operand">${bR.meta}</span>""",
            s"""</span>"""
          )
          ADD(aR, bR)(html.mkString(""), id)
        case SUB(a, b) =>
          val id = nextID()
          val aR = wrap(a, 0, env)
          val bR = wrap(b, 0, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="sub">""",
            s"""<span class="left-operand">${aR.meta}</span>""", " ",
            s"""<span class="symbol sub">-</span>""", " ",
            s"""<span class="right-operand">${bR.meta}</span>""",
            s"""</span>"""
          )
          SUB(aR, bR)(html.mkString(""), id)
        case BOOL(value) =>
          val id = nextID()
          BOOL(value)(indent(level) + s"""<span id="$id" class="bool">$value</span>""", id)
        case VAR(name) =>
          val id = nextID()
          val varID = env.get(name).getOrElse("")
          VAR(name)(indent(level) + s"""<span id="$id" class="var $varID" data-for-id="$varID">$name</span>""", id)
        case IF(test, yes, no) =>
          val id = nextID()
          val testR = loop(test, 0, env)
          val yesR = loop(yes, 0, env)
          val noR = loop(no, 0, env)
          val html = List(
            indent(level),
                           s"""<span id="$id" class="if">""",
            indent(level), s"""<span class="keyword if">if</span>""", " ",
                           s"""<span class="if-test">${testR.meta}</span>""", "\n",
            indent(level), s"""<span class="keyword then">then</span>""", " ",
                           s"""<span class="if-then">${yesR.meta}</span>""", "\n",
            indent(level), s"""<span class="keyword else">else</span>""", " ",
                           s"""<span class="if-else">${noR.meta}</span>""",
                           s"""</span>"""
          )

          IF(testR, yesR, noR)(html.mkString(""), id)
        case FN(param, body) =>
          val id = nextID()
          val paramID = nextID()
          val bodyR = loop(body, level + 1, env + (param -> paramID))
          val html = List(
            indent(level),
            s"""<span id="$id" class="fn">""",
            s"""<span class="keyword fn">fn</span>""", " ",
            s"""<span id="$paramID" class="param $paramID" data-for-id="$paramID">$param</span>""", " ",
            s"""<span class="symbol darrow">=&gt;</span>""", "\n",
            indent(level + 1), s"""<span class="fn-body">${bodyR.meta}</span>""",
            s"""</span>"""
          )
          FN(param, bodyR)(html.mkString(""), id)
        case APP(fn, arg) =>
          val id = nextID()
          val fnR = loop(fn, level, env)
          val argR = loop(arg, level, env)
          val html = List(
            indent(level),
            s"""<span id="$id" class="app">""",
            s"""<span class="app-fn">${fnR.meta}</span>""", " ",
            s"""<span class="app-arg">${argR.meta}</span>""",
            s"""</span>"""
          )
          APP(fnR, argR)(html.mkString(""), id)
        case LET(binding, value, body) =>
          val id = nextID()
          val valID = nextID()
          val valueR = loop(value, 0, env)
          val bodyR = loop(body, level + 1, env + (binding -> valID))
          val html = List(
            indent(level),
                               s"""<span id="$id" class="let">""",
                               s"""<span class="keyword let">let</span>""", "\n",
            indent(level + 1), s"""<span class="let-val">""",
                               s"""<span class="keyword val">val</span>""", " ",
                               s"""<span id="$valID" class="val-def $valID" data-for-id="$valID">$binding</span>""", " ",
                               s"""<span class="symbol equal">=</span>""", " ",
                               s"""<span class="val-value">${valueR.meta}</span>""", "\n",
                               s"""</span>""",
            indent(level),     s"""<span class="keyword in">in</span>""", "\n",
            indent(level), s"""<span class="let-body">${bodyR.meta}</span>""", "\n",
            indent(level),     s"""<span class="keyword end">end</span>""",
                               s"""</span>"""
          )
          LET(binding, valueR, bodyR)(html.mkString(""), id)
      }
    }

    loop(term, 0, Map.empty)
  }

  private def indent(level: Int): String = "  " * level
}
