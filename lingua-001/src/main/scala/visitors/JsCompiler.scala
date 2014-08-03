package lingua001
package visitors

case class Trace(returning: Boolean)

object Trace {
  def empty: Trace = Trace(returning = false)
}

trait JsCompiler extends NodeVisitor {
  override type N = Node
  override type R = String
  override type S = Trace

  private def iff[A](c: => Boolean, t: => A, f: A): A =
    if (c) t else f

  override def num(n: Int, state: S) = {
    val r = iff(state.returning, "return ", "")
    s"$r$n" -> state
  }

  override def bool(b: Boolean, state: S) = {
    val r = iff(state.returning, "return ", "")
    s"$r$b" -> state
  }

  override def add(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsa+$jsb" -> stateb
  }

  override def sub(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsa-$jsb" -> stateb
  }

  override def mul(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsa*$jsb" -> stateb
  }

  override def div(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsa/$jsb" -> stateb
  }

  override def let(id: String, value: N, body: N, state: S) = {
    val (jsValue, valueState) = value.accept(this)(state.copy(returning = false))
    val (jsBody, bodyState) = body.accept(this)(valueState.copy(returning = state.returning))
    s"var $id=$jsValue;$jsBody" -> bodyState
  }

  override def ref(id: String, state: S) = {
    val r = iff(state.returning, "return ", "")
    s"$r$id" -> state
  }

  override def fun(id: String, body: N, state: S) = {
    val (jsBody, bodyState) = body.accept(this)(state.copy(returning = true))
    val r = iff(state.returning, "return ", "")
    s"${r}function($id){$jsBody}" -> bodyState
  }

  override def app(fn: N, arg: N, state: S) = {
    val (jsFn, fnState) = fn.accept(LiteralFunctionVisitor)(state.copy(returning = false))
    val (jsArg, argState) = arg.accept(this)(fnState.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsFn($jsArg)" -> argState
  }

  override def seq(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = state.returning))
    s"$jsa;$jsb" -> stateb
  }

  override def set(id: String, value: N, state: S) = {
    val (jsValue, valueState) = value.accept(this)(state)
    s"$id=$jsValue" -> valueState
  }

  override def equal(a: N, b: N, state: S) = {
    val (jsa, statea) = a.accept(this)(state.copy(returning = false))
    val (jsb, stateb) = b.accept(this)(statea.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsa===$jsb" -> stateb
  }

  override def when(cond: N, yes: N, no: N, state: S) = {
    val (jsCond, condState) = cond.accept(this)(state.copy(returning = false))
    val (jsYes, yesState) = yes.accept(this)(condState.copy(returning = false))
    val (jsNo, noState) = no.accept(this)(yesState.copy(returning = false))
    val r = iff(state.returning, "return ", "")
    s"$r$jsCond?($jsYes):($jsNo)" -> noState
  }
}

object LiteralFunctionVisitor extends JsCompiler {
  /**
   * This visitor is only used when descending the function operator of an
   * application node and will wrap a function literal inside round paratheses.
   *
   * In all other cases, this wrapping is not necessary. This is why it is only
   * performed when descending from an `App` node.
   */
  override def fun(id: String, body: N, state: S) = {
    val (jsBody, bodyState) = body.accept(this)(state.copy(returning = true))
    s"(function($id){$jsBody})" -> bodyState
  }
}

object JsCompiler extends JsCompiler
