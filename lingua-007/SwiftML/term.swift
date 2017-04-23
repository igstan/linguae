// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Binder<Meta> {
  let name: String
  let meta: Meta
}

indirect enum Term<Meta> {
  case Num(Meta, Int)
  case Var(Meta, String)
  case Def(Meta, Binder<Meta>, Term<Meta>)
  case App(Meta, Term<Meta>, Term<Meta>)
  case Bool(Meta, Bool)
  case When(Meta, Term<Meta>, Term<Meta>, Term<Meta>)
  case Let(Meta, Binder<Meta>, Term<Meta>, Term<Meta>)

  var meta: Meta {
    switch self {
      case .Num(let meta, _): return meta
      case .Var(let meta, _): return meta
      case .Def(let meta, _, _): return meta
      case .App(let meta, _, _): return meta
      case .Bool(let meta, _): return meta
      case .When(let meta, _, _, _): return meta
      case .Let(let meta, _, _, _): return meta
    }
  }

  func eval(env: [String : Value<Meta>]) -> Result<Value<Meta>, String> {
    switch self {
      case let .Num(_, n): return Value.Num(n).success()
      case let .Var(_, n): return Result.fromOptional(env[n], "unbound variable: \(n)")
      case let .Def(_, param, body): return Value.Fun(param.name, body, env).success()
      case let .Bool(_, b): return Value.Bool(b).success()
      case let .App(_, def, arg):
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
      case let .When(_, cond, then, otherwise):
        return cond.eval(env: env).flatMap { value in
          switch value {
            case .Bool(true): return then.eval(env: env)
            case .Bool(false): return otherwise.eval(env: env)
            case _: return .Failure("boolean expected")
          }
        }
      case let .Let(_, name, value, body):
        return value.eval(env: env).flatMap { value in
          var env = env
          env[name.name] = value
          return body.eval(env: env)
        }
    }
  }
}
