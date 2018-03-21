package lingua001
package nodes

case class Div(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.div(a, b, state)
  }
}
