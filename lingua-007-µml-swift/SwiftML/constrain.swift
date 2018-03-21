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

  func solve() -> Result<Substitution, TypeError> {
    switch self {
      case .Equal(.Int, .Int): return .Success(Substitution.empty)
      case .Equal(.Bool, .Bool): return .Success(Substitution.empty)
      case let .Equal(.Fun(p1, r1), .Fun(p2, r2)):
        return Unifier.solve(constraints: [.Equal(p1, p2), .Equal(r1, r2)])
      case let .Equal(.Var(v), type): return type.solve(tvar: v)
      case let .Equal(type, .Var(v)): return type.solve(tvar: v)
      case let .Equal(a, b): return .Failure(.Conflict(a, b))
    }
  }
}

func constrain<Attr>(_ term: Term<(Attr, Type)>) -> Set<Constraint> {
  switch term {
    case .Var(_): return []
    case let .Num(attr, _): return [.Equal(attr.1, .Int)]
    case let .Bool(attr, _): return [.Equal(attr.1, .Bool)]
    case let .Def(attr, param, body):
      return constrain(body).union([
        .Equal(attr.1, .Fun(param.attr.1, body.attr.1))
      ])
    case let .App(attr, fun, arg):
      return constrain(fun).union(constrain(arg)).union([
        .Equal(fun.attr.1, .Fun(arg.attr.1, attr.1))
      ])
    case let .When(attr, cond, then, otherwise):
      return constrain(cond).union(constrain(then)).union(constrain(otherwise)).union([
        .Equal(cond.attr.1, .Bool),
        .Equal(attr.1, then.attr.1),
        .Equal(attr.1, otherwise.attr.1)
      ])
    case let .Let(attr, binder, value, body):
      return constrain(value).union(constrain(body)).union([
        .Equal(attr.1, body.attr.1),
        .Equal(binder.attr.1, value.attr.1)
      ])
  }
}
