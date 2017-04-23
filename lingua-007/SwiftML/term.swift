// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Binder<Attr> {
  let name: String
  let attr: Attr
}

indirect enum Term<Attr> {
  case Num(Attr, Int)
  case Var(Attr, String)
  case Def(Attr, Binder<Attr>, Term<Attr>)
  case App(Attr, Term<Attr>, Term<Attr>)
  case Bool(Attr, Bool)
  case When(Attr, Term<Attr>, Term<Attr>, Term<Attr>)
  case Let(Attr, Binder<Attr>, Term<Attr>, Term<Attr>)

  var attr: Attr {
    switch self {
      case .Num(let attr, _): return attr
      case .Var(let attr, _): return attr
      case .Def(let attr, _, _): return attr
      case .App(let attr, _, _): return attr
      case .Bool(let attr, _): return attr
      case .When(let attr, _, _, _): return attr
      case .Let(let attr, _, _, _): return attr
    }
  }

  func eval(env: [String : Value<Attr>]) -> Result<Value<Attr>, String> {
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
