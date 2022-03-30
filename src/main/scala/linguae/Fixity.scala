package linguae

sealed abstract class Fixity(
  val precedence: Int,
  val isL: Boolean,
  val isR: Boolean,
) extends Product with Serializable

object Fixity {
  final case class L(override val precedence: Int) extends Fixity(precedence, isL = true, isR = false)
  final case class R(override val precedence: Int) extends Fixity(precedence, isL = false, isR = true)
}
