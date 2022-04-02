// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Binder<Attr> {
  let name: String
  let attr: Attr

  func mapAttr<A>(_ fn: (Attr) -> A) -> Binder<A> {
    return Binder<A>(name: name, attr: fn(attr))
  }
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
      case let .Num(a, _): return a
      case let .Var(a, _): return a
      case let .Def(a, _, _): return a
      case let .App(a, _, _): return a
      case let .Bool(a, _): return a
      case let .When(a, _, _, _): return a
      case let .Let(a, _, _, _): return a
    }
  }

  func mapAttr<A>(_ fn: (Attr) -> A) -> Term<A> {
    switch self {
      case let .Num(attr, n): return .Num(fn(attr), n)
      case let .Var(attr, v): return .Var(fn(attr), v)
      case let .Def(attr, p, b): return .Def(fn(attr), p.mapAttr(fn), b.mapAttr(fn))
      case let .App(attr, f, a): return .App(fn(attr), f.mapAttr(fn), a.mapAttr(fn))
      case let .Bool(attr, b): return .Bool(fn(attr), b)
      case let .When(attr, c, t, o): return .When(fn(attr), c.mapAttr(fn), t.mapAttr(fn), o.mapAttr(fn))
      case let .Let(attr, n, v, b): return .Let(fn(attr), n.mapAttr(fn), v.mapAttr(fn), b.mapAttr(fn))
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
