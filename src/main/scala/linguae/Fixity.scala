package linguae

sealed trait Fixity extends Product with Serializable {
  def precedence: Int
  def appliesBefore(that: Fixity): Boolean
}

object Fixity {
  final case class L(override val precedence: Int) extends Fixity {
    override def appliesBefore(that: Fixity): Boolean =
      this.precedence <= that.precedence
  }

  final case class R(override val precedence: Int) extends Fixity {
    override def appliesBefore(that: Fixity): Boolean =
      this.precedence < that.precedence
  }
}
