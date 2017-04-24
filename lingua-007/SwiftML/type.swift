// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

indirect enum Type: Equatable, CustomStringConvertible {
  case Bool
  case Int
  case Var(Int)
  case Fun(Type, Type)

  static func ==(lhs: Type, rhs: Type) -> Bool {
    switch (lhs, rhs) {
      case (.Bool, .Bool): return true
      case (.Int, .Int): return true
      case let (.Fun(p1, r1), .Fun(p2, r2)): return p1 == p2 && r1 == r2
      case let (.Var(a), .Var(b)): return a == b
      case _: return false
    }
  }

  var hashValue: Int {
    switch self {
      case .Bool: return 0
      case .Int: return 1
      case .Var(let n): return n
      case .Fun(let p, let r): return p.hashValue ^ r.hashValue
    }
  }

  var description: String {
    switch self {
      case .Bool: return "bool"
      case .Int: return "int"
      case .Var(let v): return "var(\(v))"
      case .Fun(.Fun(let p, let q), let r): return "(\(p) -> \(q)) -> \(r)"
      case .Fun(let p, let r): return "\(p) -> \(r)"
    }
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

  func solve(tvar: Int) -> Result<Substitution, String> {
    switch self {
      case .Var(let w) where tvar == w: return .Success(Substitution.empty)
      case .Var(_): return .Success(Substitution(solutions: [tvar : self]))
      case _ where contains(tvar: tvar): return .Failure("circular use: \(tvar) occurs in \(self)")
      case _: return .Success(Substitution(solutions: [tvar : self]))
    }
  }
}
