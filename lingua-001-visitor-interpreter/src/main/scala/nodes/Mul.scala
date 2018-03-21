package lingua001
package nodes

case class Mul(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.mul(a, b, state)
  }
}
