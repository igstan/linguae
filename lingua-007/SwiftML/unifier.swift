// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

enum Unifier {
  static func solve(constraints: Set<Constraint>) -> Result<Substitution, String> {
    guard let constraint = constraints.first else {
      return .Success(Substitution.empty)
    }

    return constraint.solve().flatMap { headSubstitution in
      let tail = headSubstitution.applyTo(constraints: Set(constraints.dropFirst()))
      return solve(constraints: tail).map { tailSubstitution in
        return headSubstitution.compose(with: tailSubstitution)
      }
    }
  }
}
