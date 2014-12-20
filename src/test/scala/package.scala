package toy

import org.scalatest.matchers.{ Matcher, MatchResult }
import org.scalatest.words.ResultOfNotWordForAny

package object test {
  type FunSuite = org.scalatest.FunSuite
  type Matchers = org.scalatest.Matchers

  def evaluateTo(expected: Value) = {
    new Matcher[Result[Evaluation]] {
      def apply(actual: Result[Evaluation]) = {
        val matches = actual match {
          case Result.Success(Evaluation(actual, _)) => actual == expected
          case _ => false
        }
        MatchResult(
          matches,
          s"The value in $actual was not equal to $expected",
          s"The value in $actual was equal to $expected"
        )
      }
    }
  }
}
