package toy

object Interpreter {
  def eval(ast: Node, env: Environment, store: Store): Result[Evaluation] = ast match {
    case Num(n) => Result.Success(Evaluation(Value.Num(n), store))
    case Add(l, r) => arith(l, r, env, store, _ + _)
    case Sub(l, r) => arith(l, r, env, store, _ - _)
    case Mul(l, r) => arith(l, r, env, store, _ * _)
    case Div(l, r) => arith(l, r, env, store, _ / _)
    case If(cond, yes, no) =>
      eval(cond, env, store).flatMap {
        case Evaluation(Value.Num(0), store) => eval(no, env, store)
        case Evaluation(Value.Num(_), store) => eval(yes, env, store)
        case Evaluation(_: Value.Fun, store) =>
          Result.Failure("If condition was not a number, but a function.")
      }
    case Fun(param, body) =>
      Result.Success(Evaluation(Value.Fun(param, body, closure = env), store))
    case App(fn, arg) =>
      eval(fn, env, store).flatMap {
        case Evaluation(Value.Num(_), store) =>
          Result.Failure("Number in function application position.")
        case Evaluation(Value.Fun(param, body, closure), store) =>
          eval(arg, env, store).flatMap {
            case Evaluation(value, store) =>
              val (location, newStore) = store.add(value)
              eval(body, closure.set(param, location), newStore)
          }
      }
    case Ref(name) =>
      env.get(name).map { location =>
        store.get(location).map { value =>
          Result.Success(Evaluation(value, store))
        } getOrElse {
          Result.Failure(s"This is a bug. Identifier $name had no value at location $location.")
        }
      } getOrElse {
        Result.Failure(s"Unbound identifier: $name")
      }
    case Let(name, value, body) =>
      val (location, cell, newStore) = store.addUnbound
      val newEnv = env.set(name, location)
      eval(value, newEnv, newStore).flatMap {
        case Evaluation(value, store) =>
          cell.bind(value)
          eval(body, newEnv, store)
      }
    case Seq(a, b) =>
      eval(a, env, store).flatMap {
        case Evaluation(_, store) => eval(b, env, store)
      }
    case Set(name, value) =>
      eval(value, env, store).flatMap {
        case Evaluation(value, store) =>
          env.get(name).map { location =>
            val updatedStore = store.set(location, value)
            Result.Success(Evaluation(value, updatedStore))
          } getOrElse {
            Result.Failure(s"Unbound identifier: $name")
          }
      }
  }

  private def arith(l: Node, r: Node, env: Environment, store: Store, op: (Int, Int) => Int): Result[Evaluation] = {
    eval(l, env, store).flatMap {
      case Evaluation(_: Value.Fun, store) =>
        Result.Failure("Left operand is not a number")
      case Evaluation(Value.Num(l), store) =>
        eval(r, env, store).flatMap {
          case Evaluation(_: Value.Fun, store) =>
            Result.Failure("Right operand is not a number")
          case Evaluation(Value.Num(r), store) =>
            Result.Success(Evaluation(Value.Num(op(l, r)), store))
        }
    }
  }
}
