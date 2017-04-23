// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func annotate<Meta>(term: Term<Meta>) -> Result<Term<(Meta, Type)>, String> {
  func loop(_ term: Term<Meta>, _ env: [String:Type], _ tvarCounter: Int) -> Result<(Term<(Meta, Type)>, Int), String> {
    switch term {
      case let .Num(meta, n): return .Success(.Num((meta, .Var(tvarCounter)), n), tvarCounter + 1)
      case let .Bool(meta, n): return .Success(.Bool((meta, .Var(tvarCounter)), n), tvarCounter + 1)
      case let .Var(meta, n):
        return Result.fromOptional(env[n], "unbound identifier: \(n)").map { type in
          return (.Var((meta, type), n), tvarCounter)
        }
      case let .Def(meta, param, body):
        let defType = Type.Var(tvarCounter)
        let paramType = Type.Var(tvarCounter + 1)
        var env = env
        env[param.name] = paramType
        return loop(body, env, tvarCounter + 2).map { (typedBody, tvarCounter) in
          let binder = Binder(name: param.name, meta: (param.meta, paramType))
          return (.Def((meta, defType), binder, typedBody), tvarCounter)
        }
      case let .App(meta, fun, arg):
        let appType = Type.Var(tvarCounter)
        return loop(fun, env, tvarCounter + 1).flatMap { (fun, tvarCounter) in
          return loop(arg, env, tvarCounter).map { (arg, tvarCounter) in
            return (.App((meta, appType), fun, arg), tvarCounter + 1)
          }
        }
      case let .When(meta, cond, test, otherwise):
        let whenType = Type.Var(tvarCounter)
        return loop(cond, env, tvarCounter + 1).flatMap { (cond, tvarCounter) in
          return loop(test, env, tvarCounter).flatMap { (test, tvarCounter) in
            return loop(otherwise, env, tvarCounter).map { (otherwise, tvarCounter) in
              return (.When((meta, whenType), cond, test, otherwise), tvarCounter)
            }
          }
        }
      case let .Let(meta, name, value, body):
        let letType = Type.Var(tvarCounter)
        let valueType = Type.Var(tvarCounter + 1)
        return loop(value, env, tvarCounter + 2).flatMap { (value, tvarCounter) in
          var env = env
          env[name.name] = value.meta.1
          return loop(body, env, tvarCounter).map { (body, tvarCounter) in
            let binder = Binder(name: name.name, meta: (name.meta, valueType))
            return (.Let((meta, letType), binder, value, body), tvarCounter)
          }
        }
    }
  }

  return loop(term, [:], 0).map { $0.0 }
}
