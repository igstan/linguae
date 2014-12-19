package toy

sealed trait Node
case class Num(value: Int) extends Node
case class Add(left: Node, right: Node) extends Node
case class Sub(left: Node, right: Node) extends Node
