package lingua001
package nodes

case class Num(n: Int) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.num(n, state)
  }
}
