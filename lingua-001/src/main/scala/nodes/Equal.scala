package lingua001
package nodes

case class Equal(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.equal(a, b, state)
  }
}
