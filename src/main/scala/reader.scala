package leesp

/*
 * The tree produced by the reader.
 */
sealed trait NODE
case class ATOM(value: String) extends NODE
case class LIST(value: List[NODE]) extends NODE {
  override def toString = value.mkString("LIST(", ", ", ")")
}

/*
 * Exceptions throw by the reader.
 */
case class UnmatchedLeftParen(row: Int, col: Int) extends RuntimeException
case class UnmatchedRightParen(row: Int, col: Int) extends RuntimeException

/*
 * The reader itself!
 */
class Reader(traceExecution: Boolean) {
  def read(source: String): List[NODE] = {
    source.foldLeft(State.empty)(fold)
      .commitAtom
      .verifyUnmatchedParenthesis()
      .program
      .toList
  }

  def fold(state: State, c: Char) = {
    c match {
      case '('                            => state.commitAtom.startList.adjustPosition(c)
      case ')'                            => state.commitAtom.commitList.adjustPosition(c)
      case c if Character.isWhitespace(c) => state.commitAtom.adjustPosition(c)
      case c                              => state.adjustAtom(c).adjustPosition(c)
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
    def adjustPosition(char: Char): State = {
      if (char == '\n') {
        copy(col = 1, row = row + 1)
      } else {
        copy(col = col + 1)
      }
    }

    def commitList: State = log("commitList") {
      lastList match {
        case None => throw UnmatchedRightParen(row, col)
        case Some(list) =>
          lists match {
            case Vector() =>
              copy(
                program = program :+ LIST(list.toList),
                lastList = None,
                listPositions = listPositions.tail
              )
            case prevLists :+ x =>
              copy(
                lastList = Some(x :+ LIST(list.toList)),
                lists = prevLists,
                listPositions = listPositions.tail
              )
          }
      }
    }

    def startList: State = log("startList") {
      lastList match {
        case None => copy(
          lastList = Some(Vector.empty),
          listPositions = (row -> col) :: listPositions
        )
        case Some(list) => copy(
          lastList = Some(Vector.empty),
          lists = lists :+ list,
          listPositions = (row -> col) :: listPositions
        )
      }
    }

    def commitAtom: State = log("commitAtom") {
      lastAtom match {
        case None => this
        case Some(atom) =>
          val commited = lastList match {
            case None => copy(program = program :+ ATOM(atom))
            case Some(list) => copy(lastList = Some(list :+ ATOM(atom)))
          }
          commited.copy(lastAtom = None)
      }
    }

    def adjustAtom(char: Char): State = log(s"adjustAtom($char)") {
      lastAtom match {
        case None => copy(lastAtom = Some(char.toString))
        case Some(atom) => copy(lastAtom = Some(atom + char))
      }
    }

    def verifyUnmatchedParenthesis(): State = {
      if (lastList.isDefined) {
        listPositions match {
          case Nil => sys.error("this is a bug")
          case (row, col) :: _ => throw UnmatchedLeftParen(row, col)
        }
      }

      this
    }

    private def log[A](message: String)(result: A): A = {
      if (traceExecution) println(s"$message: $result")
      result
    }

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
