package lingua001
package visitors

trait DecoratedVisitor extends NodeVisitor {

  def decorate(result: (R, S)): (R, S)

  abstract override def num(n: Int, state: S): (R, S)                        = decorate(super.num(n, state))
  abstract override def bool(b: Boolean, state: S): (R, S)                   = decorate(super.bool(b, state))
  abstract override def add(a: N, b: N, state: S): (R, S)                    = decorate(super.add(a, b, state))
  abstract override def sub(a: N, b: N, state: S): (R, S)                    = decorate(super.sub(a, b, state))
  abstract override def mul(a: N, b: N, state: S): (R, S)                    = decorate(super.mul(a, b, state))
  abstract override def div(a: N, b: N, state: S): (R, S)                    = decorate(super.div(a, b, state))
  abstract override def let(id: String, value: N, body: N, state: S): (R, S) = decorate(super.let(id, value, body, state))
  abstract override def ref(id: String, state: S): (R, S)                    = decorate(super.ref(id, state))
  abstract override def fun(id: String, body: N, state: S): (R, S)           = decorate(super.fun(id, body, state))
  abstract override def app(fn: N, arg: N, state: S): (R, S)                 = decorate(super.app(fn, arg, state))
  abstract override def seq(a: N, b: N, state: S): (R, S)                    = decorate(super.seq(a, b, state))
  abstract override def set(id: String, value: N, state: S): (R, S)          = decorate(super.set(id, value, state))
  abstract override def equal(a: N, b: N, state: S): (R, S)                  = decorate(super.equal(a, b, state))
  abstract override def when(cond: N, yes: N, no: N, state: S): (R, S)       = decorate(super.when(cond, yes, no, state))
}

trait SomeTrait {
  def foo(a: Int): Int
  def bar(a: Int): Int
  def baz(a: Int): Int
}

trait DecoratedTrait extends SomeTrait {
  def decorate(a: Int): Int
  abstract override def foo(a: Int) = decorate(super.foo(a))
  abstract override def bar(a: Int) = decorate(super.foo(a))
  abstract override def baz(a: Int) = decorate(super.foo(a))
}
