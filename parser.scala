package leesp

import Character.isWhitespace

sealed trait NODE
case class ATOM(value: String) extends NODE
case class LIST(value: List[NODE]) extends NODE {
  override def toString = value.mkString("LIST(", ", ", ")")
}

object Parser {
  val traceExecution = false

  def main(args: Array[String]): Unit = {
    showProgram(args(0))
    showProgram("""
      (define a 1)
      (define b 2)
      (define add (lambda (a b) (+ a b)))
      (add a b)
    """)
  }

  def showProgram(source: String): Unit = {
    println(parse(source).mkString("\n"))
  }

  def parse(source: String): List[NODE] = {
    commitAtom(source.foldLeft(State.empty)(folder)).program.toList
  }

  type Transition = State => State

  def folder(state: State, char: Char) = {
    char match {
      case '('                  => startList(commitAtom(state))
      case ')'                  => commitList(commitAtom(state))
      case c if isWhitespace(c) => commitAtom(state)
      case c                    => adjustAtom(c)(state)
    }
  }

  def log(message: String)(transition: Transition): Transition = { state =>
    val changedState = transition(state)
    if (traceExecution) println(s"$message: $changedState")
    changedState
  }

  val commitList: Transition = log("commitList") { state =>
    state.lastList match {
      case None => sys.error("unbalanced parentheses")
      case Some(list) =>
        state.lists match {
          case Vector() =>
            state.copy(program = state.program :+ LIST(list.toList), lastList = None)
          case prevLists :+ x =>
            state.copy(lastList = Some(x :+ LIST(list.toList)), lists = prevLists)
        }
    }
  }

  val startList: Transition = log("startList") { state =>
    state.lastList match {
      case None => state.copy(lastList = Some(Vector.empty))
      case Some(list) => state.copy(lastList = Some(Vector.empty), lists = state.lists :+ list)
    }
  }

  val commitAtom: Transition = log("commitAtom") { state =>
    state.lastAtom match {
      case None => state
      case Some(atom) =>
        val commited = state.lastList match {
          case None => state.copy(program = state.program :+ ATOM(atom))
          case Some(list) => state.copy(lastList = Some(list :+ ATOM(atom)))
        }
        commited.copy(lastAtom = None)
    }
  }

  def adjustAtom(char: Char): Transition = log(s"adjustAtom($char)") { state =>
    state.lastAtom match {
      case None => state.copy(lastAtom = Some(char.toString))
      case Some(atom) => state.copy(lastAtom = Some(atom + char))
    }
  }

  case class State(
    program: Vector[NODE],
    lists: Vector[Vector[NODE]],
    lastList: Option[Vector[NODE]],
    lastAtom: Option[String]
  ) {
    override def toString = {
      s"""State(
      |   program = $program,
      |     lists = $lists,
      |  lastList = $lastList,
      |  lastAtom = $lastAtom
      |)""".stripMargin
    }
  }

  object State {
    def empty = State(Vector.empty, Vector.empty, Option.empty, Option.empty)
  }
}
