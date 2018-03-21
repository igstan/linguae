package lingua001
package nodes

case class App(fn: Node, arg: Node) extends Node {
  override def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S) = {
    visitor.app(fn, arg, state)
  }
}
