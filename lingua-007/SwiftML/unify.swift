// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func unify(_ constraints: Set<Constraint>) -> Result<Substitution, String> {
  switch constraints.first {
    case .none: return .Success(Substitution.empty)
    case .some(let constraint):
      return unifyOne(constraint).flatMap { substHead in
        let tail = substHead.applyTo(constraints: Set(constraints.dropFirst()))
        return unify(tail).map { substTail in
          return substHead.compose(with: substTail)
        }
      }
  }
}

func unifyOne(_ constraint: Constraint) -> Result<Substitution, String> {
  switch constraint {
    case .Equal(.Int, .Int): return .Success(Substitution.empty)
    case .Equal(.Bool, .Bool): return .Success(Substitution.empty)
    case let .Equal(.Fun(p1, r1), .Fun(p2, r2)):
      return unify([.Equal(p1, p2), .Equal(r1, r2)])
    case let .Equal(.Var(v), type): return unifyVar(type, v)
    case let .Equal(type, .Var(v)): return unifyVar(type, v)
    case let .Equal(a, b): return .Failure("cannot unify \(a) with \(b)")
  }
}

func unifyVar(_ type: Type, _ v: Int) -> Result<Substitution, String> {
  switch type {
    case .Var(let w) where v == w: return .Success(Substitution.empty)
    case .Var(_): return .Success(Substitution(solutions: [v : type]))
    case _ where type.contains(tvar: v): return .Failure("circular use: \(v) occurs in \(type)")
    case _: return .Success(Substitution(solutions: [v : type]))
  }
}
