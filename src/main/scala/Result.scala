package toy

sealed trait Result
object Result {
  case class Success(value: Value) extends Result
  case class Failure(reason: String) extends Result
}
