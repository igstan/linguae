//
//  SwiftML
//

func parse(_ tokens: [Token]) -> (Term, Tokens)? {
  func parseWhen(_ tokens: Tokens) -> (Term, Tokens)? {
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
                        return (.When(cond, then, otherwise), tokens)
                    }
                  case _: return nil
                }
            }
          case _: return nil
        }
    }
  }

  func parseApp(_ tokens: Tokens) -> (Term, Tokens)? {
    func loop(_ exp: Term, _ tokens: Tokens) -> (Term, Tokens)? {
      switch parseAtexp(tokens) {
        case .none: return (exp, tokens)
        case .some(let arg, let tokens): return loop(.App(exp, arg), tokens)
      }
    }

    switch parseAtexp(tokens) {
      case .none: return nil
      case .some(let exp, let tokens): return loop(exp, tokens)
    }
  }

  func parseFn(_ tokens: Tokens) -> (Term, Tokens)? {
    switch tokens.first {
      case .some(.ID(let param)):
        let tokens = tokens.dropFirst()
        switch tokens.first {
          case .some(.DARROW):
            switch parseExpr(tokens.dropFirst()) {
              case .none: return nil
              case .some(let body, let tokens): return (.Def(param, body), tokens)
            }
          case _: return nil
        }
      case _: return nil
    }
  }

  func parseExpr(_ tokens: Tokens) -> (Term, Tokens)? {
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

  func parseParens(_ tokens: Tokens) -> (Term, Tokens)? {
    switch parseExpr(tokens) {
      case .none: return nil
      case .some(let exp, let tokens):
        switch tokens.first {
          case .some(.RPAREN): return (exp, tokens.dropFirst())
          case _: return nil
        }
    }
  }

  func parseLet(_ tokens: Tokens) -> (Term, Tokens)? {
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
                            return (.Let(name, value, body), tokens)
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

  func parseAtexp(_ tokens: Tokens) -> (Term, Tokens)? {
    switch tokens.first {
      case .none: return nil
      case .some(let token):
        switch token {
          case .NUM(let n): return (.Num(n), tokens.dropFirst())
          case .TRUE: return (.Bool(true), tokens.dropFirst())
          case .FALSE: return (.Bool(false), tokens.dropFirst())
          case .ID(let id): return (.Var(id), tokens.dropFirst())
          case .LPAREN: return parseParens(tokens.dropFirst())
          case .LET: return parseLet(tokens.dropFirst())
          case _: return nil
        }
    }
  }

  return parseExpr(ArraySlice(tokens))
}
