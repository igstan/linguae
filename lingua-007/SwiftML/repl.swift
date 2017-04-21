// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func repl(prompt: String) {
  print(prompt, terminator: "")

  switch readLine() {
    case .none, .some(":q"), .some(":quit"):
      print("Bye!")
      return
    case .some(let source):
      switch parse(scanAll(source.characters)) {
        case .none: print("syntax error")
        case let .some((term, _)):
          switch term.eval(env: [:]) {
            case .Failure(let f): print("error:", f)
            case .Success(let s): print("val it =", s.toString())
          }
      }
      repl(prompt: prompt)
  }
}
