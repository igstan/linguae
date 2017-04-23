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

  func compose(with: Substitution) -> Substitution {
    var substituted = solutions.mapValues { value in
      return with.applyTo(type: value)
    }

    for (tvar, type) in with.solutions {
      substituted[tvar] = type
    }

    return Substitution(solutions: substituted)
  }
}
