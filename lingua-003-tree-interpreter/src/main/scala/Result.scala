package toy

sealed trait Result[+A] {
  import Result._

  def map[B](fn: A => B): Result[B] = {
    this match {
      case Success(s) => Success(fn(s))
      case Failure(f) => Failure(f)
    }
  }

  def flatMap[B](fn: A => Result[B]): Result[B] = {
    this match {
      case Success(s) => fn(s)
      case Failure(f) => Failure(f)
    }
  }
}

object Result {
  case class Success[A](value: A) extends Result[A]
  case class Failure(reason: String) extends Result[Nothing]
}
