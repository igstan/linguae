// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

enum Constraint: Hashable, CustomStringConvertible {
  case Equal(Type, Type)

  static func ==(lhs: Constraint, rhs: Constraint) -> Bool {
    switch (lhs, rhs) {
      case let (Equal(a1, b1), Equal(a2, b2)): return a1 == a2 && b1 == b2
    }
  }

  var hashValue: Int {
    switch self {
      case let .Equal(r, p): return r.hashValue ^ p.hashValue
    }
  }

  var description: String {
    switch self {
      case let .Equal(a, b): return "\(a) ≣ \(b)"
    }
  }

  func solve() -> Result<Substitution, String> {
    switch self {
      case .Equal(.Int, .Int): return .Success(Substitution.empty)
      case .Equal(.Bool, .Bool): return .Success(Substitution.empty)
      case let .Equal(.Fun(p1, r1), .Fun(p2, r2)):
        return Unifier.solve(constraints: [.Equal(p1, p2), .Equal(r1, r2)])
      case let .Equal(.Var(v), type): return type.solve(tvar: v)
      case let .Equal(type, .Var(v)): return type.solve(tvar: v)
      case let .Equal(a, b): return .Failure("cannot unify \(a) with \(b)")
    }
  }
}

func constrain<Meta>(_ term: Term<(Meta, Type)>) -> Set<Constraint> {
  switch term {
    case .Var(_): return []
    case let .Num(meta, _): return [.Equal(meta.1, .Int)]
    case let .Bool(meta, _): return [.Equal(meta.1, .Bool)]
    case let .Def(meta, param, body):
      return constrain(body).union([
        .Equal(meta.1, .Fun(param.meta.1, body.meta.1))
      ])
    case let .App(meta, fun, arg):
      return constrain(fun).union(constrain(arg)).union([
        .Equal(fun.meta.1, .Fun(arg.meta.1, meta.1))
      ])
    case let .When(meta, cond, then, otherwise):
      return constrain(cond).union(constrain(then)).union(constrain(otherwise)).union([
        .Equal(cond.meta.1, .Bool),
        .Equal(meta.1, then.meta.1),
        .Equal(meta.1, otherwise.meta.1)
      ])
    case let .Let(meta, binder, value, body):
      return constrain(value).union(constrain(body)).union([
        .Equal(meta.1, body.meta.1),
        .Equal(binder.meta.1, value.meta.1)
      ])
  }
}
