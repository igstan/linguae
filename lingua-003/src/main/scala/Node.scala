package toy

sealed trait Node
case class Num(value: Int) extends Node
case class Add(left: Node, right: Node) extends Node
case class Sub(left: Node, right: Node) extends Node
case class Mul(left: Node, right: Node) extends Node
case class Div(left: Node, right: Node) extends Node
case class If(cond: Node, yes: Node, no: Node) extends Node
case class Fun(param: String, body: Node) extends Node
case class App(fn: Node, arg: Node) extends Node
case class Ref(name: String) extends Node
case class Let(name: String, value: Node, body: Node) extends Node
case class Seq(a: Node, b: Node) extends Node
case class Set(name: String, value: Node) extends Node
