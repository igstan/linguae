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
      switch scanAll(source.characters) {
        case .Failure(let f): print(f)
        case .Success(let tokens):
          switch parse(tokens) {
            case .Failure(let failure): print("parse error: \(failure)")
            case .Success(let term, _):
              let r = elaborate(term: term).flatMap { typedTerm in
                return typedTerm.eval(env: [:]).map { value in
                  return (typedTerm.attr.1, value)
                }
              }

              switch r {
                case .Failure(let f): print("type error:", f)
                case .Success(let (type, value)): print("val it = \(value) : \(type)")
              }
        }
      }

      repl(prompt: prompt)
  }
}
