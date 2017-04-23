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
      case .Fun(let p, let r):
        switch p {
          case .Fun: return "(\(p)) → \(r)"
          case _: return "\(p) → \(r)"
        }
    }
  }

  func substitute(tvar: Int, with: Type) -> Type {
    switch self {
      case .Int, .Bool: return self
      case let .Fun(p, r):
        return .Fun(
          p.substitute(tvar: tvar, with: with),
          r.substitute(tvar: tvar, with: with)
        )
      case let .Var(w) where tvar == w: return with
      case .Var(_): return self
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

struct Binder {
  let name: String
  let type: Type
}

indirect enum TypedTerm {
  case Num(Type, Int)
  case Var(Type, String)
  case Def(Type, Binder, TypedTerm)
  case App(Type, TypedTerm, TypedTerm)
  case Bool(Type, Bool)
  case When(Type, TypedTerm, TypedTerm, TypedTerm)
  case Let(Type, Binder, TypedTerm, TypedTerm)

  func type() -> Type {
    switch self {
      case let .Num(type, _): return type
      case let .Var(type, _): return type
      case let .Def(type, _, _): return type
      case let .App(type, _, _): return type
      case let .Bool(type, _): return type
      case let .When(type, _, _, _): return type
      case let .Let(type, _, _, _): return type
    }
  }
}

func annotate(term: Term) -> Result<TypedTerm, String> {
  func loop(_ term: Term, _ env: [String:Type], _ tvarCounter: Int) -> Result<(TypedTerm, Int), String> {
    switch term {
      case let .Num(n): return .Success(.Num(.Var(tvarCounter), n), tvarCounter + 1)
      case let .Bool(n): return .Success(.Bool(.Var(tvarCounter), n), tvarCounter + 1)
      case let .Var(n):
        return Result.fromOptional(env[n], "unbound identifier: \(n)").map { type in
          return (.Var(type, n), tvarCounter)
        }
      case let .Def(param, body):
        let defType = Type.Var(tvarCounter)
        let paramType = Type.Var(tvarCounter + 1)
        var env = env
        env[param] = paramType
        return loop(body, env, tvarCounter + 2).map { (typedBody, tvarCounter) in
          let binder = Binder(name: param, type: paramType)
          return (.Def(defType, binder, typedBody), tvarCounter)
        }
      case let .App(fun, arg):
        let appType = Type.Var(tvarCounter)
        return loop(fun, env, tvarCounter + 1).flatMap { (fun, tvarCounter) in
          return loop(arg, env, tvarCounter).map { (arg, tvarCounter) in
            return (TypedTerm.App(appType, fun, arg), tvarCounter + 1)
          }
        }
      case let .When(cond, test, otherwise):
        let whenType = Type.Var(tvarCounter)
        return loop(cond, env, tvarCounter + 1).flatMap { (cond, tvarCounter) in
          return loop(test, env, tvarCounter).flatMap { (test, tvarCounter) in
            return loop(otherwise, env, tvarCounter).map { (otherwise, tvarCounter) in
              return (.When(whenType, cond, test, otherwise), tvarCounter)
            }
          }
        }
      case let .Let(name, value, body):
        let letType = Type.Var(tvarCounter)
        let valueType = Type.Var(tvarCounter + 1)
        return loop(value, env, tvarCounter + 2).flatMap { (value, tvarCounter) in
          var env = env
          env[name] = value.type()
          return loop(body, env, tvarCounter).map { (body, tvarCounter) in
            let binder = Binder(name: name, type: valueType)
            return (.Let(letType, binder, value, body), tvarCounter)
          }
        }
    }
  }

  return loop(term, [:], 0).map { $0.0 }
}
