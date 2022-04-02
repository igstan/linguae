package linguae

sealed trait Fixity extends Product with Serializable {
  def precedence: Int
  def appliesAfter(that: Fixity): Boolean
}

object Fixity {
  final case class L(override val precedence: Int) extends Fixity {
    override def appliesAfter(that: Fixity): Boolean =
      this.precedence <= that.precedence
  }

  final case class R(override val precedence: Int) extends Fixity {
    override def appliesAfter(that: Fixity): Boolean =
      this.precedence < that.precedence
  }
}
