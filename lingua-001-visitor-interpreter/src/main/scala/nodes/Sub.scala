package lingua001
package nodes

case class Sub(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.sub(a, b, state)
  }
}
