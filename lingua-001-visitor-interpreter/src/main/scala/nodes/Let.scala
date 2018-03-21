package lingua001
package nodes

case class Let(id: String, value: Node, body: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.let(id, value, body, state)
  }
}

object Let {
  def apply(id: String, value: Node)(body: => Node): Let = Let(id, value, body)
}
