package toy

sealed trait Node
case class Num(value: Int) extends Node
