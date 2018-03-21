package lingua001
package nodes

case class Ref(id: String) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.ref(id, state)
  }
}
