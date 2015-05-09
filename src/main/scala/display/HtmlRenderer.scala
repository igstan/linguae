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

    def wrap(term: Term, level: Int): Term = {
      term match {
        case APP(fn, arg) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="app"><span class="left-paren">(</span>""",
            s"""<span class="app-fn">${loop(fn, level).meta}</span>""", " ",
            s"""<span class="app-arg">${loop(arg, level).meta}</span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          APP(fn, arg)(html.mkString(""))
        case ADD(a, b) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="add"><span class="left-paren">(</span>""",
            s"""<span class="left-operand">${wrap(a, 0).meta}<span>""", " ",
            s"""<span class="symbol add">+<span>""", " ",
            s"""<span class="right-operand">${wrap(b, 0).meta}<span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          ADD(a, b)(html.mkString(""))
        case SUB(a, b) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="sub"><span class="left-paren">(</span>""",
            s"""<span class="left-operand">${wrap(a, 0).meta}<span>""", " ",
            s"""<span class="symbol sub">0<span>""", " ",
            s"""<span class="right-operand">${wrap(b, 0).meta}<span>""",
            s"""<span class="right-paren">)</span></span>"""
          )
          SUB(a, b)(html.mkString(""))
        case other => loop(other, level)
      }
    }

    def loop(term: Term, level: Int): Term = {
      term match {
        case INT(value) =>
          val id = nextID()
          INT(value)(s"""<span id="$id" class="int">$value</span>""")
        case ADD(a, b) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="add">""",
            s"""<span class="left-operand">${wrap(a, 0).meta}<span>""", " ",
            s"""<span class="symbol add">+<span>""", " ",
            s"""<span class="right-operand">${wrap(b, 0).meta}<span>""",
            s"""</span>"""
          )
          ADD(a, b)(html.mkString(""))
        case SUB(a, b) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="sub">""",
            s"""<span class="left-operand">${wrap(a, 0).meta}<span>""", " ",
            s"""<span class="symbol sub">-<span>""", " ",
            s"""<span class="right-operand">${wrap(b, 0).meta}<span>""",
            s"""</span>"""
          )
          SUB(a, b)(html.mkString(""))
        case BOOL(value) =>
          val id = nextID()
          BOOL(value)(s"""<span id="$id" class="bool">$value</span>""")
        case VAR(name) =>
          val id = nextID()
          VAR(name)(s"""<span id="$id" class="var">$name</span>""")
        case IF(test, yes, no) =>
          val id = nextID()
          val html = List(
                           s"""<span id="$id" class="if">""",
            indent(level), s"""<span class="keyword if">if</span>""", " ",
                           s"""<span class="if-test">${loop(test, 0).meta}</span>""", "\n",
                           s"""<span class="keyword then">then</span>""", " ",
            indent(level), s"""<span class="if-then">${loop(yes, 0).meta}</span>""", "\n",
                           s"""<span class="keyword else">else</span>""", " ",
            indent(level), s"""<span class="if-else">${loop(no, 0).meta}</span>""", "\n",
                           s"""</span>"""
          )

          IF(test, yes, no)(html.mkString(""))
        case FN(param, body) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="fn">""",
            s"""<span class="keyword fn">fn</span>""", " ",
            s"""<span class="param">$param</span>""", " ",
            s"""<span class="symbol darrow">=&gt;</span>""", "\n",
            indent(level + 2), s"""<span class="fn-body">${loop(body, level + 2).meta}</span>""",
            s"""</span>"""
          )
          FN(param, body)(html.mkString(""))
        case APP(fn, arg) =>
          val id = nextID()
          val html = List(
            s"""<span id="$id" class="app">""",
            s"""<span class="app-fn">${loop(fn, level).meta}</span>""", " ",
            s"""<span class="app-arg">${loop(arg, level).meta}</span>""",
            s"""</span>"""
          )
          APP(fn, arg)(html.mkString(""))
        case LET(binding, value, body) =>
          val id = nextID()
          val html = List(
                               s"""<span id="$id" class="let">""",
                               s"""<span class="keyword let">let</span>""", "\n",
            indent(level + 1), s"""<span class="let-val">""",
                               s"""<span class="keyword val">val</span>""", " ",
                               s"""<span class="val-def">$binding</span>""", " ",
                               s"""<span class="symbol equal">=</span>""", " ",
                               s"""<span class="val-value">${loop(value, level).meta}</span>""", "\n",
                               s"""</span>""",
            indent(level),     s"""<span class="keyword in">in</span>""", "\n",
            indent(level + 1), s"""<span class="let-body">${loop(body, level).meta}</span>""", "\n",
            indent(level),     s"""<span class="keyword end">end</span>""",
                               s"""</span>"""
          )
          LET(binding, value, body)(html.mkString(""))
      }
    }

    loop(term, 0)
  }

  private def indent(level: Int): String = "  " * level
}
