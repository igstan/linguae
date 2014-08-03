package lingua001
package nodes

case class Fun(id: String, body: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.fun(id, body, state)
  }
}
