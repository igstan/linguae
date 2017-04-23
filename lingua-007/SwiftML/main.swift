// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

// repl(prompt: "SwiftML> ")

let source = "fn zero => when zero 0 then 1 else 2".characters
let tokens = scanAll(source)

switch parse(tokens) {
  case .none: print("unknown parse error")
  case .some(let term, let tokens):
    switch annotate(term: term) {
      case .Failure(let failure): print("error \(failure)")
      case .Success(let annotated):
        print("annotated: \(annotated)")
        let constraints = constrain(annotated)
        print("constraints: \(constraints)")
        let solutions = Unifier.solve(constraints: constraints)
        print("solutions: \(solutions)")
    }
}
