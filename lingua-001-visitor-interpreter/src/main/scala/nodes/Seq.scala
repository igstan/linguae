package lingua001
package nodes

case class Seq(a: Node, b: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.seq(a, b, state)
  }
}
