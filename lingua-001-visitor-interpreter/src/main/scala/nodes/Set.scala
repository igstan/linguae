package lingua001
package nodes

case class Set(id: String, value: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.set(id, value, state)
  }
}
