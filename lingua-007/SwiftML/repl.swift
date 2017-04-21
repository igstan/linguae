//
//  SwiftML
//

func repl(prompt: String) {
  print(prompt, terminator: "")

  switch readLine() {
    case .none, .some(":q"), .some(":quit"):
      print("Bye!")
      return
    case .some(let source):
      switch parse(scanAll(source.characters)) {
        case .none:
          print("syntax error")
        case let .some((term, _)):
          print("val it =", String(describing: term.eval(env: [:])))
      }
      repl(prompt: prompt)
  }
}
