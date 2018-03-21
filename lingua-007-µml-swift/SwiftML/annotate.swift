// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func annotate<Attr>(term: Term<Attr>) -> Result<Term<(Attr, Type)>, TypeError> {
  func loop(_ term: Term<Attr>, _ env: [String:Type], _ tvarCounter: Int) -> Result<(Term<(Attr, Type)>, Int), TypeError> {
    switch term {
      case let .Num(attr, n): return .Success((.Num((attr, .Var(tvarCounter)), n), tvarCounter + 1))
      case let .Bool(attr, n): return .Success((.Bool((attr, .Var(tvarCounter)), n), tvarCounter + 1))
      case let .Var(attr, n):
        return Result.fromOptional(env[n], TypeError.Unbound(identifier: n)).map { type in
          (.Var((attr, type), n), tvarCounter)
        }
      case let .Def(attr, param, body):
        let defType = Type.Var(tvarCounter)
        let paramType = Type.Var(tvarCounter + 1)
        var env = env
        env[param.name] = paramType
        return loop(body, env, tvarCounter + 2).map { (typedBody, tvarCounter) in
          let binder = Binder(name: param.name, attr: (param.attr, paramType))
          return (.Def((attr, defType), binder, typedBody), tvarCounter)
        }
      case let .App(attr, fun, arg):
        let appType = Type.Var(tvarCounter)
        return loop(fun, env, tvarCounter + 1).flatMap { (fun, tvarCounter) in
          loop(arg, env, tvarCounter).map { (arg, tvarCounter) in
            (.App((attr, appType), fun, arg), tvarCounter + 1)
          }
        }
      case let .When(attr, cond, test, otherwise):
        let whenType = Type.Var(tvarCounter)
        return loop(cond, env, tvarCounter + 1).flatMap { (cond, tvarCounter) in
          loop(test, env, tvarCounter).flatMap { (test, tvarCounter) in
            loop(otherwise, env, tvarCounter).map { (otherwise, tvarCounter) in
              (.When((attr, whenType), cond, test, otherwise), tvarCounter)
            }
          }
        }
      case let .Let(attr, name, value, body):
        let letType = Type.Var(tvarCounter)
        let valueType = Type.Var(tvarCounter + 1)
        var env = env
        env[name.name] = valueType
        return loop(value, env, tvarCounter + 2).flatMap { (value, tvarCounter) in
          return loop(body, env, tvarCounter).map { (body, tvarCounter) in
            let binder = Binder(name: name.name, attr: (name.attr, valueType))
            return (.Let((attr, letType), binder, value, body), tvarCounter)
          }
        }
    }
  }

  return loop(term, [:], 0).map { $0.0 }
}
