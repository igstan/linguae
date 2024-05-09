// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

indirect enum Type: Hashable, CustomStringConvertible {
  case Bool
  case Int
  case Var(Int)
  case Fun(Type, Type)

  static func ==(lhs: Type, rhs: Type) -> Bool {
    switch (lhs, rhs) {
      case (.Bool, .Bool): return true
      case (.Int, .Int): return true
      case let (.Fun(f1, a1), .Fun(f2, a2)): return f1 == f2 && a1 == a2
      case let (.Var(a), .Var(b)): return a == b
      case _: return false
    }
  }

  func hash(into hasher: inout Hasher) {
    switch self {
      case .Bool: hasher.combine(0)
      case .Int: hasher.combine(1)
      case .Var(let n): hasher.combine(n)
      case .Fun(let p, let r):
        hasher.combine(p)
        hasher.combine(r)
    }
  }

  private static let letters = Array(Substring("abcdefghijklmnopqrstuvwxyz"))

  private func letter(_ n: Int) -> String {
    func recur(_ n: Int, _ str: String) -> String {
      let letter = Type.letters[n % Type.letters.count]
      let result = String(letter) + str
      return n <= 25 ? result : recur(n / Type.letters.count - 1, result)
    }

    return recur(n, "")
  }

  var description: String {
    func traverse(type: Type, vars: [Int : String]) -> (desc: String, vars: [Int : String]) {
      switch type {
        case .Bool: return ("bool", vars)
        case .Int: return ("int", vars)
        case .Var(let v):
          var vars2 = vars
          let tvar = vars2.get(key: v, orUpdate: letter(vars.count))
          return ("'\(tvar)", vars2)
        case .Fun(let p, let r):
          let b = traverse(type: r, vars: vars)
          let a = traverse(type: p, vars: b.vars)
          switch p {
            case .Fun: return ("(\(a.desc)) -> \(b.desc)", a.vars)
            case _: return ("\(a.desc) -> \(b.desc)", a.vars)
          }
      }
    }

    return traverse(type: self, vars: [:]).desc
  }

  func substitute(tvar: Int, with: Type) -> Type {
    switch self {
      case let .Fun(p, r):
        let p = p.substitute(tvar: tvar, with: with)
        let r = r.substitute(tvar: tvar, with: with)
        return .Fun(p, r)
      case let .Var(w) where tvar == w: return with
      case .Int, .Bool, .Var(_): return self
    }
  }

  func contains(tvar: Int) -> Bool {
    switch self {
      case let .Fun(p, r): return p.contains(tvar: tvar) || r.contains(tvar: tvar)
      case let .Var(w): return w == tvar
      case _: return false
    }
  }

  func solve(tvar: Int) -> Result<Substitution, TypeError> {
    switch self {
      case .Var(let w) where tvar == w: return .Success(Substitution.empty)
      case .Var(_): return .Success(Substitution(solutions: [tvar : self]))
      case _ where contains(tvar: tvar): return .Failure(.Circular(tvar, self))
      case _: return .Success(Substitution(solutions: [tvar : self]))
    }
  }
}

enum Constraint: Hashable, CustomStringConvertible {
  case Equal(Type, Type)

  static func ==(lhs: Constraint, rhs: Constraint) -> Bool {
    switch (lhs, rhs) {
      case let (Equal(a1, b1), Equal(a2, b2)): return a1 == a2 && b1 == b2
    }
  }

  func hash(into hasher: inout Hasher) {
    switch self {
      case let .Equal(r, p):
        hasher.combine(r)
        hasher.combine(p)
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
    case .Var(_, _): return []
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
