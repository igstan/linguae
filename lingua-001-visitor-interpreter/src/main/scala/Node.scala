package lingua001

trait Node {
  def accept(visitor: NodeVisitor { type N = Node })(state: visitor.S): (visitor.R, visitor.S)
}
