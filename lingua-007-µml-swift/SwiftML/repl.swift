// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func repl(prompt: String) {
  print(prompt, terminator: "")

  switch readLine() {
    case .none, .some(":q"), .some(":quit"):
      print("Bye!")
      return
    case .some(let source) where source.isEmpty: repl(prompt: prompt)
    case .some(let source):
      switch scanAll(Substring(source)) {
        case .Failure(let f): print(f)
        case .Success(let tokens):
          switch parse(tokens) {
            case .Failure(let failure): print("parse error: \(failure)")
            case .Success(let term, _):
              switch elaborate(term: term) {
                case let .Failure(.Circular(a, b)): print("circular use: \(a) and \(b)")
                case let .Failure(.Unbound(id)): print("unbound identifier: \(id)")
                case let .Failure(.Conflict(a, b)): print("cannot unify \(a) with \(b)")
                case let .Success(typedTerm):
                  switch typedTerm.eval(env: [:]) {
                    case let .Failure(failure): print("eval error: \(failure)")
                    case let .Success(value): print("val it = \(value) : \(typedTerm.attr.1)")
                  }
              }
        }
      }

      repl(prompt: prompt)
  }
}
