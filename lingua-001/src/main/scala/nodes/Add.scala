package lingua001
package nodes

case class Add(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.add(a, b, state)
  }
}
