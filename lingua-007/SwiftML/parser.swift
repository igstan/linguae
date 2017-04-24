// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

struct Position {
  let row: Int
  let col: Int

  static let unknown = Position(row: -1, col: -1)
}

func parseAll(tokens: [Token]) -> Result<[Term<Position>], String> {
  func recur(_ tokens: [Token], _ terms: [Term<Position>]) -> Result<[Term<Position>], String> {
    if tokens.isEmpty {
      return .Success(terms)
    } else {
      switch parse(tokens) {
        case .Failure(let f): return .Failure(f)
        case let .Success(term, tokens):
          var terms = terms
          terms.append(term)
          return recur(Array(tokens), terms)
      }
    }
  }

  return recur(tokens, [])
}

func parse(_ tokens: [Token]) -> Result<(Term<Position>, Tokens), String> {
  func parseWhen(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    return parseExpr(tokens).flatMap { (cond, tokens) in
      switch tokens.first {
        case .some(.THEN):
          return parseExpr(tokens.dropFirst()).flatMap { (then, tokens) in
            switch tokens.first {
              case .some(.ELSE):
                return parseExpr(tokens.dropFirst()).map { (otherwise, tokens) in
                  let pos = Position.unknown
                  return (.When(pos, cond, then, otherwise), tokens)
                }
              case _: return .Failure("expected ELSE")
            }
          }
        case _: return .Failure("expected THEN")
      }
    }
  }

  func parseApp(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    func loop(_ exp: Term<Position>, _ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
      switch parseAtexp(tokens) {
        case .Failure(_): return .Success(exp, tokens)
        case .Success(let arg, let tokens):
          let pos = Position.unknown
          return loop(.App(pos, exp, arg), tokens)
      }
    }

    return parseAtexp(tokens).flatMap(loop)
  }

  func parseFn(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    switch tokens.first {
      case .some(.ID(let param)):
        let tokens = tokens.dropFirst()
        switch tokens.first {
          case .some(.DARROW):
            return parseExpr(tokens.dropFirst()).map { (body, tokens) in
              let pos = Position.unknown
              let param = Binder(name: param, attr: pos)
              return (.Def(pos, param, body), tokens)
            }
          case _: return .Failure("expected DARROW")
        }
      case _: return .Failure("expected identifier")
    }
  }

  func parseExpr(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    switch parseApp(tokens) {
      case .Success(let term, let tokens): return .Success(term, tokens)
      case .Failure(let failure):
        switch tokens.first {
          case .some(.FN): return parseFn(tokens.dropFirst())
          case .some(.WHEN): return parseWhen(tokens.dropFirst())
          case _: return .Failure(failure)
        }
    }
  }

  func parseParens(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    return parseExpr(tokens).flatMap { (exp, tokens) in
      switch tokens.first {
        case .some(.RPAREN): return .Success(exp, tokens.dropFirst())
        case _: return .Failure("expected RPAREN")
      }
    }
  }

  func parseLet(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    switch tokens.first {
      case .some(.VAL):
        let tokens = tokens.dropFirst()
        switch tokens.first {
          case .some(.ID(let name)):
            let tokens = tokens.dropFirst()
            switch tokens.first {
              case .some(.EQUAL):
                return parseExpr(tokens.dropFirst()).flatMap { (value, tokens) in
                  switch tokens.first {
                    case .some(.IN):
                      return parseExpr(tokens.dropFirst()).map { (body, tokens) in
                        let pos = Position.unknown
                        let name = Binder(name: name, attr: pos)
                        return (.Let(pos, name, value, body), tokens)
                      }
                    case _: return .Failure("expected IN")
                  }
                }
              case _: return .Failure("expected EQUAL")
            }
          case _: return .Failure("expected identifier")
        }
      case _: return .Failure("expected VAL")
    }
  }

  func parseAtexp(_ tokens: Tokens) -> Result<(Term<Position>, Tokens), String> {
    switch tokens.first {
      case .none: return .Failure("expected number, true, false, identifier, LPAREN or LET")
      case .some(let token):
        let pos = Position.unknown
        switch token {
          case .NUM(let n): return .Success(.Num(pos, n), tokens.dropFirst())
          case .TRUE: return .Success(.Bool(pos, true), tokens.dropFirst())
          case .FALSE: return .Success(.Bool(pos, false), tokens.dropFirst())
          case .ID(let id): return .Success(.Var(pos, id), tokens.dropFirst())
          case .LPAREN: return parseParens(tokens.dropFirst())
          case .LET: return parseLet(tokens.dropFirst())
          case _: return .Failure("expected number, true, false, identifier, LPAREN or LET")
        }
    }
  }

  return parseExpr(ArraySlice(tokens))
}
