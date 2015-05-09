package ro.igstan.debugger
package eval

case class Resumption(env: Env)(val next: () => Resumption.Step)

object Resumption {
  sealed trait Step
  case class Done(result: Result) extends Step
  case class Next(resumption: Resumption) extends Step
}
