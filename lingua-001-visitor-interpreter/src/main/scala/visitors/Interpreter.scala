package lingua001
package visitors

sealed trait Result[+S, +F] {
  def map[SS](fn: S => SS): Result[SS, F] = {
    this match {
      case Success(s) => Success(fn(s))
      case Failure(f) => Failure(f)
    }
  }

  def flatMap[SS, FF >: F](fn: S => Result[SS, FF]): Result[SS, FF] = {
    this match {
      case Success(s) => fn(s)
      case Failure(f) => Failure(f)
    }
  }
}

case class Success[S](success: S) extends Result[S, Nothing]
case class Failure[F](failure: F) extends Result[Nothing, F]

sealed trait Value
object Value {
  case class Num(n: Int) extends Value
  case class Bool(b: Boolean) extends Value
  case class Fun(id: String, body: Node, closure: Interpreter.Environment) extends Value
}

object Interpreter {
  type Location = Int
  type Environment = List[(String, Location)]
  type Store = Map[Location, Value]

  val Environment = List
  val Store = Map

  object Visitor extends NodeVisitor {
    val log = logger("Interpreter.Visitor")

    type Error = String

    override type N = Node
    override type R = Result[Value, Error]
    override type S = (Environment, Store)

    override def num(n: Int, state: S) = {
      Success(Value.Num(n)) -> state
    }

    override def bool(b: Boolean, state: S) = {
      Success(Value.Bool(b)) -> state
    }

    private def arithmetic(a: N, b: N, state: S, op: (Int, Int) => Int) = {
      val (lresult, lstate) = a.accept(this)(state)
      val (rresult, rstate) = b.accept(this)(lstate)

      (lresult, rresult) match {
        case (f @ Failure(_), _) => f -> rstate
        case (_, f @ Failure(_)) => f -> rstate
        case (Success(Value.Num(l)), Success(Value.Num(r))) => Success(Value.Num(op(l, r))) -> rstate
        case (Success(s), _) => Failure(s"left addition operand is not a number: $s") -> rstate
        case (_, Success(s)) => Failure(s"right addition operand is not a number: $s") -> rstate
      }
    }

    override def add(a: N, b: N, state: S) = arithmetic(a, b, state, _ + _)
    override def sub(a: N, b: N, state: S) = arithmetic(a, b, state, _ - _)
    override def mul(a: N, b: N, state: S) = arithmetic(a, b, state, _ * _)
    override def div(a: N, b: N, state: S) = arithmetic(a, b, state, _ / _)

    override def let(id: String, value: N, body: N, state: S) = {
      val (vresult, vstate) = value.accept(this)(state)
      vresult match {
        case Success(s) =>
          val (env, store) = vstate
          val location = store.size
          val newStore = store + (location -> s)
          val newEnv = (id -> location) :: env
          body.accept(this)(newEnv -> newStore)
        case Failure(_) => vresult -> vstate
      }
    }

    override def ref(id: String, state: S) = {
      val (env, store) = state
      val r = env.find(_._1 == id)
        .map { case (_, loc) =>
          store.get(loc)
            .map(Success(_))
            .getOrElse(Failure(s"identifier not in store: '$id'"))
        }
        .getOrElse(Failure(s"unbound identifier: '$id'"))

      r -> state
    }

    override def fun(id: String, body: N, state: S) = {
      val (env, store) = state
      Success(Value.Fun(id, body, env)) -> state
    }

    override def app(fnNode: N, argNode: N, state: S) = {
      val (fn, state1) = fnNode.accept(this)(state)

      fn match {
        case Success(Value.Fun(id, body, closure)) =>
          val (arg, state2) = argNode.accept(this)(state1)

          arg match {
            case Failure(_) => arg -> state2
            case Success(arg) =>
              val (env, store) = state2
              val location = store.size
              val newEnv = (id -> location) :: closure
              val newStore = store + (location -> arg)
              val (result, localState) = body.accept(this)(newEnv -> newStore)
              val storeAfterExecution = localState._2
              // Drop the local env created while executing the function
              // body, but keep the (possibly update) store.
              result -> (env -> storeAfterExecution)
          }
        case Success(_) => Failure(s"operator is not a function: $fn") -> state1
        case Failure(_) => fn -> state1
      }
    }

    override def seq(a: N, b: N, state: S) = {
      val (resultA, stateA) = a.accept(this)(state)

      resultA match {
        case Failure(_) => resultA -> stateA
        case Success(_) => b.accept(this)(stateA)
      }
    }

    override def set(id: String, value: N, state: S) = {
      val (resultA, stateA) = value.accept(this)(state)

      resultA match {
        case Failure(_) => resultA -> stateA
        case Success(s) =>
          val (env, store) = stateA

          // Lookup env location for `id`
          val envEntry = env.find(_._1 == id)

          envEntry match {
            case None => Failure(s"unbound identifier: '$id'") -> stateA
            case Some((id, location)) =>
              store.get(location) match {
                case None => Failure(s"identifier not in store: '$id'") -> stateA
                case Some(value) =>
                  val newStore = store + (location -> s)
                  Success(value) -> (env -> newStore)
              }
          }
      }
    }

    override def equal(nodeA: N, nodeB: N, state: S) = {
      val (a, stateA) = nodeA.accept(this)(state)

      a match {
        case Failure(_) => a -> stateA
        case Success(Value.Num(_)) | Success(Value.Bool(_)) =>
          val (b, stateB) = nodeB.accept(this)(stateA)

          b match {
            case Failure(_) => b -> stateB
            case Success(Value.Num(_)) | Success(Value.Bool(_)) =>
              Success(Value.Bool(a == b)) -> stateB
            case Success(b) => Failure(s"right eq operand is not a number: $b") -> stateB
          }
        case Success(a) => Failure(s"left eq operand is not a number: $a") -> stateA
      }
    }

    override def when(condNode: N, yesNode: N, noNode: N, state: S) = {
      val (cond, condState) = condNode.accept(this)(state)

      cond match {
        case Failure(_) => cond -> condState
        case Success(cond) =>
          cond match {
            case Value.Bool(cond) if  cond => yesNode.accept(this)(condState)
            case Value.Bool(cond) if !cond => noNode.accept(this)(condState)
            case _ => Failure(s"condition in if expression is not of type boolean") -> condState
          }
      }
    }
  }
}
