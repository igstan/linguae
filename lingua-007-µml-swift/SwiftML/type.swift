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
      case let (.Fun(f1), .Fun(f2)): return f1 == f2
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
          var vars = vars
          let tvar = vars.get(key: v, orUpdate: letter(vars.count))
          return ("'\(tvar)", vars)
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
