package lingua001

trait NodeVisitor {
  type N // Node
  type R // Result
  type S // State

  def num(n: Int, state: S): (R, S)
  def bool(b: Boolean, state: S): (R, S)
  def add(a: N, b: N, state: S): (R, S)
  def sub(a: N, b: N, state: S): (R, S)
  def mul(a: N, b: N, state: S): (R, S)
  def div(a: N, b: N, state: S): (R, S)
  def let(id: String, value: N, body: N, state: S): (R, S)
  def ref(id: String, state: S): (R, S)
  def fun(id: String, body: N, state: S): (R, S)
  def app(fn: N, arg: N, state: S): (R, S)
  def seq(a: N, b: N, state: S): (R, S)
  def set(id: String, value: N, state: S): (R, S)
  def equal(a: N, b: N, state: S): (R, S)
  def when(cond: N, yes: N, no: N, state: S): (R, S)
}
