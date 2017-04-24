// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func elaborate<A>(term: Term<A>) -> Result<Term<(A, Type)>, TypeError> {
  return annotate(term: term).flatMap { annotated in
    return Unifier.solve(constraints: constrain(annotated)).map { substitution in
      return substitution.applyTo(term: annotated)
    }
  }
}
