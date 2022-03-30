package linguae

import scala.annotation.tailrec

final class Parser[A](val run: String => Either[String, (A, String)]) extends AnyVal {

  def consume(s: String): Either[String, A] =
    run(s) match {
      case Right((a, rest)) if rest.isEmpty => Right(a)
      case Right((_, rest)) => Left(s"extra characters: $rest")
      case Left(error) => Left(error)
    }

  def as[B](b: B): Parser[B] = map(_ => b)

  def void: Parser[Unit] = as(())

  def *> [B](parser: Parser[B]): Parser[B] =
    flatMap(_ => parser)

  def <* [B](parser: Parser[B]): Parser[A] =
    flatMap(b => parser.as(b))

  def map[B](fn: A => B): Parser[B] =
    Parser { stream =>
      run(stream).map {
        case (a, rest) => (fn(a), rest)
      }
    }

  def flatMap[B](fn: A => Parser[B]): Parser[B] =
    Parser { stream =>
      run(stream).flatMap {
        case (a, rest) => fn(a).run(rest)
      }
    }

  def | (that: Parser[A]): Parser[A] =
    Parser { string =>
      run(string) match {
        case Left(_) => that.run(string) // backtracks
        case right => right
      }
    }

  def optional: Parser[Option[A]] =
    Parser { string =>
      run(string) match {
        case Left(_) => Right((None, string))
        case Right((a, rest)) => Right((Some(a), rest))
      }
    }

  def many: Parser[List[A]] =
    Parser { string =>
      val result = List.newBuilder[A]

      @tailrec
      def loop(rest: String): Either[String, (List[A], String)] =
        run(rest) match {
          case Left(error) =>
            val list = result.result()
            if (list.isEmpty) Left(error) else Right((list, rest))

          case Right((a, rest)) =>
            result += a
            loop(rest)
        }

      loop(string)
    }

  def optionalMany: Parser[List[A]] =
    Parser { string =>
      val result = List.newBuilder[A]

      @tailrec
      def loop(string: String): Either[String, (List[A], String)] =
        run(string) match {
          case Left(_) =>
            Right((result.result(), string))
          case Right((a, rest)) =>
            result += a
            loop(rest)
        }

      loop(string)
    }
}

object Parser {
  def apply[A](run: String => Either[String, (A, String)]): Parser[A] =
    new Parser(run)

  def success[A](a: A): Parser[A] =
    Parser(s => Right((a, s)))

  def failure[A](error: String): Parser[A] =
    Parser(_ => Left(error))

  def char(c: Char): Parser[Char] = satisfy(_ == c)

  def satisfy(predicate: Char => Boolean): Parser[Char] =
    Parser {
      case "" => Left("empty input")
      case st =>
        val char = st.charAt(0)

        if (predicate(char)) {
          Right((char, st.drop(1)))
        } else {
          Left(s"$char didn't match predicate")
        }
    }
}
