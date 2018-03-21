package lingua001
package nodes

case class When(cond: Node, yes: Node, no: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.when(cond, yes, no, state)
  }
}
