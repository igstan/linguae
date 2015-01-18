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
    commitAtom(source.foldRight(State.empty)(folder)).program
  }

  type Transition = State => State

  def folder(char: Char, state: State) = {
    char match {
      case '('                  => commitList(commitAtom(state))
      case ')'                  => startList(commitAtom(state))
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
          case Nil =>
            state.copy(program = LIST(list) :: state.program, lastList = None)
          case x :: prevLists =>
            state.copy(lastList = Some(LIST(list) :: x), lists = prevLists)
        }
    }
  }

  val startList: Transition = log("startList") { state =>
    state.lastList match {
      case None => state.copy(lastList = Some(List.empty))
      case Some(list) => state.copy(lastList = Some(List.empty), lists = list :: state.lists)
    }
  }

  val commitAtom: Transition = log("commitAtom") { state =>
    state.lastAtom match {
      case None => state
      case Some(atom) =>
        val commited = state.lastList match {
          case None => state.copy(program = ATOM(atom) :: state.program)
          case Some(list) => state.copy(lastList = Some(ATOM(atom) :: list))
        }
        commited.copy(lastAtom = None)
    }
  }

  def adjustAtom(char: Char): Transition = log(s"adjustAtom($char)") { state =>
    state.lastAtom match {
      case None => state.copy(lastAtom = Some(char.toString))
      case Some(atom) => state.copy(lastAtom = Some(char + atom))
    }
  }

  case class State(
    program: List[NODE],
    lists: List[List[NODE]],
    lastList: Option[List[NODE]],
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
    def empty = State(List.empty, List.empty, Option.empty, Option.empty)
  }
}
