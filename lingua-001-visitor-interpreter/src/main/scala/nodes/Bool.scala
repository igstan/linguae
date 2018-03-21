package lingua001
package nodes

case class Bool(b: Boolean) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.bool(b, state)
  }
}
