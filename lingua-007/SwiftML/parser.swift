// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Position {
  let row: Int
  let col: Int

  static let unknown = Position(row: -1, col: -1)
}

func parse(_ tokens: [Token]) -> (Term<Position>, Tokens)? {
  func parseWhen(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch parseExpr(tokens) {
      case .none: return nil
      case .some(let cond, let tokens):
        switch tokens.first {
          case .some(.THEN):
            switch parseExpr(tokens.dropFirst()) {
              case .none: return nil
              case .some(let then, let tokens):
                switch tokens.first {
                  case .some(.ELSE):
                    switch parseExpr(tokens.dropFirst()) {
                      case .none: return nil
                      case .some(let otherwise, let tokens):
                        let pos = Position.unknown
                        return (.When(pos, cond, then, otherwise), tokens)
                    }
                  case _: return nil
                }
            }
          case _: return nil
        }
    }
  }

  func parseApp(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    func loop(_ exp: Term<Position>, _ tokens: Tokens) -> (Term<Position>, Tokens)? {
      switch parseAtexp(tokens) {
        case .none: return (exp, tokens)
        case .some(let arg, let tokens):
          let pos = Position.unknown
          return loop(.App(pos, exp, arg), tokens)
      }
    }

    switch parseAtexp(tokens) {
      case .none: return nil
      case .some(let exp, let tokens): return loop(exp, tokens)
    }
  }

  func parseFn(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch tokens.first {
      case .some(.ID(let param)):
        let tokens = tokens.dropFirst()
        switch tokens.first {
          case .some(.DARROW):
            switch parseExpr(tokens.dropFirst()) {
              case .none: return nil
              case .some(let body, let tokens):
                let pos = Position.unknown
                let param = Binder(name: param, meta: pos)
                return (.Def(pos, param, body), tokens)
            }
          case _: return nil
        }
      case _: return nil
    }
  }

  func parseExpr(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch parseApp(tokens) {
      case .some(let term, let tokens): return (term, tokens)
      case .none:
        switch tokens.first {
          case .some(.FN): return parseFn(tokens.dropFirst())
          case .some(.WHEN): return parseWhen(tokens.dropFirst())
          case _: return nil
        }
    }
  }

  func parseParens(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch parseExpr(tokens) {
      case .none: return nil
      case .some(let exp, let tokens):
        switch tokens.first {
          case .some(.RPAREN): return (exp, tokens.dropFirst())
          case _: return nil
        }
    }
  }

  func parseLet(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch tokens.first {
      case .some(.VAL):
        let tokens = tokens.dropFirst()
        switch tokens.first {
          case .some(.ID(let name)):
            let tokens = tokens.dropFirst()
            switch tokens.first {
              case .some(.EQUAL):
                switch parseExpr(tokens.dropFirst()) {
                  case .some(let value, let tokens):
                    switch tokens.first {
                      case .some(.IN):
                        switch parseExpr(tokens.dropFirst()) {
                          case .some(let body, let tokens):
                            let pos = Position.unknown
                            let name = Binder(name: name, meta: pos)
                            return (.Let(pos, name, value, body), tokens)
                          case _: return nil
                        }
                      case _: return nil
                    }
                  case _: return nil
                }
              case _: return nil
            }
          case _: return nil
        }
      case _: return nil
    }
  }

  func parseAtexp(_ tokens: Tokens) -> (Term<Position>, Tokens)? {
    switch tokens.first {
      case .none: return nil
      case .some(let token):
        let pos = Position.unknown
        switch token {
          case .NUM(let n): return (.Num(pos, n), tokens.dropFirst())
          case .TRUE: return (.Bool(pos, true), tokens.dropFirst())
          case .FALSE: return (.Bool(pos, false), tokens.dropFirst())
          case .ID(let id): return (.Var(pos, id), tokens.dropFirst())
          case .LPAREN: return parseParens(tokens.dropFirst())
          case .LET: return parseLet(tokens.dropFirst())
          case _: return nil
        }
    }
  }

  return parseExpr(ArraySlice(tokens))
}
