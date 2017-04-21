//
//  SwiftML
//

indirect enum Value {
  case Num(Int)
  case Bool(Bool)
  case Fun(String, Term, [String:Value])
  case Primitive((Value) -> Result<Value, String>)

  func success() -> Result<Value, String> {
    return .Success(self)
  }

  func toString() -> String {
    switch self {
      case .Num(let n): return String(n)
      case .Bool(true): return "true"
      case .Bool(false): return "false"
      case .Fun(_, _, _), .Primitive(_): return "fn"
    }
  }
}

indirect enum Term {
  case Num(Int)
  case Var(String)
  case Def(String, Term)
  case App(Term, Term)
  case Bool(Bool)
  case When(Term, Term, Term)
  case Let(String, Term, Term)

  func eval(env: [String:Value]) -> Result<Value, String> {
    switch self {
      case let .Num(n): return Value.Num(n).success()
      case let .Var(n): return Result.fromOptional(env[n], "unbound variable: \(n)")
      case let .Def(param, body): return Value.Fun(param, body, env).success()
      case let .Bool(b): return Value.Bool(b).success()
      case let .App(def, arg):
        return def.eval(env: env).flatMap { def in
          switch def {
            case .Primitive(let fn): return arg.eval(env: env).flatMap(fn)
            case .Fun(let param, let body, var closure):
              return arg.eval(env: env).flatMap { arg in
                closure[param] = arg
                return body.eval(env: closure)
              }
            case _: return .Failure("function required")
          }
        }
      case let .When(cond, then, otherwise):
        return cond.eval(env: env).flatMap { value in
          switch value {
            case .Bool(true): return then.eval(env: env)
            case .Bool(false): return otherwise.eval(env: env)
            case _: return .Failure("boolean expected")
          }
        }
      case let .Let(name, value, body):
        return value.eval(env: env).flatMap { value in
          var env = env
          env[name] = value
          return body.eval(env: env)
        }
    }
  }
}
