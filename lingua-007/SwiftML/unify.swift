// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

typealias Substitution = [Int : Type]

func unify(_ constraints: Set<Constraint>) -> Result<Substitution, String> {
  switch constraints.first {
    case .none: return .Success([:])
    case .some(let constraint):
      return unifyOne(constraint).flatMap { substHead in
        let tail = applySubst(substHead, Set(constraints.dropFirst()))
        return unify(tail).map { substTail in
          return composeSubst(substHead, substTail)
        }
      }
  }
}

func unifyOne(_ constraint: Constraint) -> Result<Substitution, String> {
  switch constraint {
    case .Equal(.Int, .Int): return .Success([:])
    case .Equal(.Bool, .Bool): return .Success([:])
    case let .Equal(.Fun(p1, r1), .Fun(p2, r2)):
      return unify([.Equal(p1, p2), .Equal(r1, r2)])
    case let .Equal(.Var(v), type): return unifyVar(type, v)
    case let .Equal(type, .Var(v)): return unifyVar(type, v)
    case let .Equal(a, b): return .Failure("cannot unify \(a) with \(b)")
  }
}

func unifyVar(_ type: Type, _ v: Int) -> Result<Substitution, String> {
  switch type {
    case .Var(let w) where v == w: return .Success([:])
    case .Var(_): return .Success([v : type])
    case _ where occurs(v, type): return .Failure("circular use: \(v) occurs in \(type)")
    case _: return .Success([v : type])
  }
}

func occurs(_ v: Int, _ type: Type) -> Bool {
  switch type {
    case let .Fun(p, r): return occurs(v, p) || occurs(v, r)
    case let .Var(w): return v == w
    case _: return false
  }
}

func applySubst(_ subst: Substitution, _ constraints: Set<Constraint>) -> Set<Constraint> {
  return Set(constraints.map { constraint in
    switch constraint {
      case let .Equal(a, b):
        let a = applyTo(subst, type: a)
        let b = applyTo(subst, type: b)
        return .Equal(a, b)
    }
  })
}

func applyTo(_ subst: Substitution, type: Type) -> Type {
  return subst.reduce(type) { (result, solution) in
    let (tvar, solutionType) = solution
    return substitute(result, tvar, solutionType)
  }
}

func substitute(_ type: Type, _ v: Int, _ replace: Type) -> Type {
  switch type {
    case .Int, .Bool: return type
    case let .Fun(p, r):
      return .Fun(substitute(p, v, replace), substitute(r, v, replace))
    case let .Var(w) where v == w: return replace
    case .Var(_): return type
  }
}

func composeSubst(_ a: Substitution, _ b: Substitution) -> Substitution {
  var substituted = a.mapValues { value in
    return applyTo(b, type: value)
  }

  for (tvar, type) in b {
    substituted[tvar] = type
  }

  return substituted
}
