// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

enum Unifier {
  static func solve(constraints: Set<Constraint>) -> Result<Substitution, String> {
    switch constraints.first {
      case .none: return .Success(Substitution.empty)
      case .some(let constraint):
        return constraint.solve().flatMap { substHead in
          let tail = substHead.applyTo(constraints: Set(constraints.dropFirst()))
          return solve(constraints: tail).map { substTail in
            return substHead.compose(with: substTail)
          }
        }
    }
  }
}
