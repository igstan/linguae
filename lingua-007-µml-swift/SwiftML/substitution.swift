// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Substitution {
  let solutions: [Int : Type]

  static let empty = Substitution(solutions: [:])

  func applyTo(constraints: Set<Constraint>) -> Set<Constraint> {
    return Set(constraints.map(applyTo(constraint:)))
  }

  func applyTo(constraint: Constraint) -> Constraint {
    switch constraint {
    case let .Equal(a, b):
      return .Equal(applyTo(type: a), applyTo(type: b))
    }
  }

  func applyTo(type: Type) -> Type {
    return solutions.reduce(type) { (result, solution) in
      let (tvar, solutionType) = solution
      return result.substitute(tvar: tvar, with: solutionType)
    }
  }

  func applyTo<A>(term: Term<(A, Type)>) -> Term<(A, Type)> {
    return term.mapAttr { (a, type) in (a, applyTo(type: type)) }
  }

  func compose(with other: Substitution) -> Substitution {
    var substituted = solutions.mapValues { other.applyTo(type: $0) }

    for (tvar, type) in other.solutions {
      substituted[tvar] = type
    }

    return Substitution(solutions: substituted)
  }
}
