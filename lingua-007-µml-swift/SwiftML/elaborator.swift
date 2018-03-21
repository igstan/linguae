// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func elaborate<A>(term: Term<A>) -> Result<Term<(A, Type)>, TypeError> {
  return annotate(term: term).flatMap { annotated in
    Unifier.solve(constraints: constrain(annotated)).map { substitution in
      substitution.applyTo(term: annotated)
    }
  }
}
