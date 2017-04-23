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
}

func constrain(_ term: TypedTerm) -> Set<Constraint> {
  switch term {
    case .Var(_): return []
    case let .Num(type, _): return [.Equal(type, .Int)]
    case let .Bool(type, _): return [.Equal(type, .Bool)]
    case let .Def(type, param, body):
      return constrain(body).union([
        .Equal(type, .Fun(param.type, body.type()))
      ])
    case let .App(type, fun, arg):
      return constrain(fun).union(constrain(arg)).union([
        .Equal(fun.type(), .Fun(arg.type(), type))
      ])
    case let .When(type, cond, then, otherwise):
      return constrain(cond).union(constrain(then)).union(constrain(otherwise)).union([
        .Equal(cond.type(), .Bool),
        .Equal(type, then.type()),
        .Equal(type, otherwise.type())
      ])
    case let .Let(type, binder, value, body):
      return constrain(value).union(constrain(body)).union([
        .Equal(type, body.type()),
        .Equal(binder.type, value.type())
      ])
  }
}
