// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

enum Unifier {
  static func solve(constraints: Set<Constraint>) -> Result<Substitution, String> {
    guard let constraint = constraints.first else {
      return .Success(Substitution.empty)
    }

    return constraint.solve().flatMap { substHead in
      let tail = substHead.applyTo(constraints: Set(constraints.dropFirst()))
      return solve(constraints: tail).map { substTail in
        return substHead.compose(with: substTail)
      }
    }
  }
}
