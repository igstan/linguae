package leesp

import Character.isWhitespace

sealed trait NODE
case class ATOM(value: String) extends NODE
case class LIST(value: List[NODE]) extends NODE {
  override def toString = value.mkString("LIST(", ", ", ")")
}

case class UnmatchedLeftParen(row: Int, col: Int) extends RuntimeException
case class UnmatchedRightParen(row: Int, col: Int) extends RuntimeException

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
    try {
      println(parse(source).mkString("\n"))
    } catch {
      case UnmatchedLeftParen(row, col) =>
        println(s"unmatched left parenthesis: row=$row, col=$col")
      case UnmatchedRightParen(row, col) =>
        println(s"unmatched right parenthesis: row=$row, col=$col")
    }
  }

  def parse(source: String): List[NODE] = {
    val finalState = commitAtom(source.foldLeft(State.empty)(transition))
    verifyUnmatchedParenthesis(finalState)
    finalState.program.toList
  }

  def verifyUnmatchedParenthesis(state: State): Unit = {
    if (state.lastList.isDefined) {
      state.listPositions match {
        case Nil => sys.error("this is a bug")
        case (row, col) :: _ => throw UnmatchedLeftParen(row, col)
      }
    }
  }

  type Transition = State => State

  def transition(state: State, char: Char) = {
    adjustPosition(char) {
      char match {
        case '('                  => startList(commitAtom(state))
        case ')'                  => commitList(commitAtom(state))
        case c if isWhitespace(c) => commitAtom(state)
        case c                    => adjustAtom(c)(state)
      }
    }
  }

  def adjustPosition(char: Char): Transition = { state =>
    if (char == '\n') {
      state.copy(col = 1, row = state.row + 1)
    } else {
      state.copy(col = state.col + 1)
    }
  }

  def log(message: String)(transition: Transition): Transition = { state =>
    val changedState = transition(state)
    if (traceExecution) println(s"$message: $changedState")
    changedState
  }

  val commitList: Transition = log("commitList") { state =>
    state.lastList match {
      case None => throw UnmatchedRightParen(state.row, state.col)
      case Some(list) =>
        state.lists match {
          case Vector() =>
            state.copy(
              program = state.program :+ LIST(list.toList),
              lastList = None,
              listPositions = state.listPositions.tail
            )
          case prevLists :+ x =>
            state.copy(
              lastList = Some(x :+ LIST(list.toList)),
              lists = prevLists,
              listPositions = state.listPositions.tail
            )
        }
    }
  }

  val startList: Transition = log("startList") { state =>
    state.lastList match {
      case None => state.copy(
        lastList = Some(Vector.empty),
        listPositions = (state.row -> state.col) :: state.listPositions
      )
      case Some(list) => state.copy(
        lastList = Some(Vector.empty),
        lists = state.lists :+ list,
        listPositions = (state.row -> state.col) :: state.listPositions
      )
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
    lastAtom: Option[String],
    listPositions: List[(Int, Int)],
    row: Int,
    col: Int
  ) {
    override def toString = {
      s"""State(
      |       program = $program,
      |         lists = $lists,
      |      lastList = $lastList,
      |      lastAtom = $lastAtom
      | listPositions = $listPositions,
      |           row = $row,
      |           col = $col
      |)""".stripMargin
    }
  }

  object State {
    def empty = State(
      program = Vector.empty,
      lists = Vector.empty,
      lastList = Option.empty,
      lastAtom = Option.empty,
      listPositions = List.empty,
      row = 1,
      col = 1
    )
  }
}
