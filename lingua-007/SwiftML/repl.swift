//
//  SwiftML
//

func repl(prompt: String) {
  repeat {
    print(prompt, terminator: "")
    switch readLine() {
      case .none: return
      case .some(":q"), .some(":quit"):
        print("Bye!")
        return
      case .some(let source):
        switch parse(scanAll(source.characters)) {
          case .none: print("syntax error")
          case let .some((term, _)):
            print("term:", String(describing: term.eval(env: [:])))
        }
    }
  } while (true)
}
