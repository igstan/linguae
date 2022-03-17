package peval

import java.io.{ BufferedReader, ByteArrayInputStream, InputStream, InputStreamReader }
import java.lang.Character.isWhitespace

/*
 * The tree produced by the reader.
 */
sealed trait NODE
case class ATOM(value: String) extends NODE
case class LIST(value: NODE*) extends NODE

/*
 * Exceptions throw by the reader.
 */
final case class UnbalancedParenL(row: Int, col: Int)
  extends RuntimeException(s"row: $row; col: $col")
final case class UnbalancedParenR(row: Int, col: Int)
  extends RuntimeException(s"row: $row; col: $col")

object Reader {
  def read(source: String): List[NODE] =
    read(new ByteArrayInputStream(source.getBytes("UTF-8")))

  def read(stream: InputStream): List[NODE] = {
    val reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"))
    var program = Vector.empty[NODE]
    var pendingList = List.empty[Vector[NODE]]
    var pendingAtom = ""
    var insideComment = false
    var col = 1
    var row = 1

    def adjustPosition(char: Char): Unit =
      if (char == '\n') {
        col = 1
        row = row + 1
      } else {
        col = col + 1
      }

    def commitAtom(): Unit =
      if (pendingAtom.nonEmpty) {
        pendingList match {
          case Nil => program = program :+ ATOM(pendingAtom)
          case h :: t => pendingList = (h :+ ATOM(pendingAtom)) :: t
        }

        pendingAtom = ""
      }

    def adjustAtom(char: Char): Unit = pendingAtom += char

    def startList(): Unit =
      pendingList = Vector.empty[NODE] :: pendingList

    def commitList(): Unit =
      pendingList match {
        case Nil => throw UnbalancedParenR(row, col)
        case list :: Nil =>
          program = program :+ LIST(list: _*)
          pendingList = List.empty
        case list :: h :: t =>
          pendingList = (h :+ LIST(list: _*)) :: t
      }

    try {
      var char = reader.read()

      while (char > -1) {
        if (insideComment) {
          if (char == '\n') {
            insideComment = false
          }
        } else {
          char match {
            case ';' => insideComment = true
            case '[' => commitAtom(); startList()
            case ']' => commitAtom(); commitList()
            case chr =>
              if (isWhitespace(chr)) commitAtom() else adjustAtom(chr.toChar)
          }
        }

        adjustPosition(char.toChar)
        char = reader.read()
      }
    } finally {
      reader.close()
    }

    commitAtom()

    if (pendingList.nonEmpty) {
      throw UnbalancedParenL(row, col - 1)
    }

    program.toList
  }
}
