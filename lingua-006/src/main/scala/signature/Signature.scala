package codecamp
package signature

import scala.collection.mutable

object Signature {
  def forType(ty: Type): String = {
    val varNames = Iterator.from(0).map(nameFromNumber)
    val knownVars = mutable.Map.empty[Type.Var, String]

    def traverse(ty: Type): String = {
      ty match {
        case Type.INT => "int"
        case Type.BOOL => "bool"
        case Type.VAR(v) => knownVars.getOrElseUpdate(v, varNames.next())
        case Type.FUN(param, ret) =>
          val paramSignature = traverse(param)
          val returnSignature = traverse(ret)
          param match {
            case _: Type.FUN => s"($paramSignature) -> $returnSignature"
            case _ => s"$paramSignature -> $returnSignature"
          }
      }
    }

    traverse(ty)
  }

  private def nameFromNumber(counter: Int): String = {
    @annotation.tailrec
    def recur(counter: Int, suffix: String): String = {
      val result = (97 + counter % 26).toChar +: suffix

      if (counter >= 0 && counter <= 25) {
        result
      } else {
        recur(counter / 26 - 1, result)
      }
    }

    "'" + recur(counter, "")
  }
}
