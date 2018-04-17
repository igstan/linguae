// -------------------------------------------------------------------------- //
// Swift ML                                                                   //
// -------------------------------------------------------------------------- //

enum Token {
  case ID(String)
  case NUM(Int)
  case DARROW
  case LPAREN
  case RPAREN
  case WHEN
  case THEN
  case ELSE
  case FALSE
  case TRUE
  case FN
  case LET
  case IN
  case END
  case VAL
  case EQUAL
}

typealias Stream = Substring
typealias Tokens = Array<Token>.SubSequence

let alphaChars = Set("abcdefghijklmnopqrstuvwxyz")
let digitChars = Set("0123456789")
let spaceChars = Set(" \n\r\t")
let tokens: [String: Token] = [
  "else": .ELSE,
  "end": .END,
  "false": .FALSE,
  "fn": .FN,
  "in": .IN,
  "let": .LET,
  "then": .THEN,
  "true": .TRUE,
  "val": .VAL,
  "when": .WHEN,
]

func isAlpha(_ char: Character) -> Bool {
  return alphaChars.contains(char)
}

func isDigit(_ char: Character) -> Bool {
  return digitChars.contains(char)
}

func isSpace(_ char: Character) -> Bool {
  return spaceChars.contains(char)
}

func identifier(_ stream: Stream) -> (String, Stream)? {
  func loop(_ stream: Stream, _ acc: Stream) -> (String, Stream)? {
    switch stream.first {
      case .some(let c) where isAlpha(c):
        var acc = acc
        acc.append(c)
        return loop(stream.dropFirst(), acc)
      case _: return (String(acc), stream)
    }
  }

  switch stream.first {
    case .some(let c) where isAlpha(c): return loop(stream.dropFirst(), Substring(String(c)))
    case _: return nil
  }
}

func number(_ stream: Stream) -> (Int, Stream)? {
  func loop(_ stream: Stream, _ acc: Stream) -> (Int, Stream)? {
    switch stream.first {
      case .some(let c) where isDigit(c):
        var acc = acc
        acc.append(c)
        return loop(stream.dropFirst(), acc)
      case _: return (Int(String(acc), radix: 10)!, stream)
    }
  }

  switch stream.first {
    case .some(let c) where isDigit(c): return loop(stream.dropFirst(), Substring(String(c)))
    case _: return nil
  }
}

func scan(source: Stream) -> Result<(Token, Stream), String>? {
  switch source.first {
    case .none: return nil
    case .some("("): return .Success((.LPAREN, source.dropFirst()))
    case .some(")"): return .Success((.RPAREN, source.dropFirst()))
    case .some("="):
      let source = source.dropFirst()
      switch source.first {
        case .some(">"): return .Success((.DARROW, source.dropFirst()))
        case _: return .Success((.EQUAL, source))
      }
    case .some(let char):
      if let (id, rest) = identifier(source) {
        let token = tokens[id] ?? .ID(id)
        return .Success((token, rest))
      } else {
        switch number(source) {
          case .some(let n, let rest): return .Success((.NUM(n), rest))
          case .none: return .Failure("unexpected char: \(char)")
        }
      }
  }
}

func scanAll(_ source: Stream) -> Result<[Token], String> {
  func loop(_ source: Stream, _ acc: [Token]) -> Result<[Token], String> {
    let trimmed = source.drop(while: isSpace)

    if trimmed.isEmpty {
      return .Success(acc)
    } else {
      switch scan(source: trimmed) {
        case .none: return .Success(acc)
        case .some(.Failure(let f)): return .Failure(f)
        case .some(.Success(let token, let rest)):
          var acc = acc
          acc.append(token)
          return loop(rest, acc)
      }
    }
  }

  return loop(source, [])
}
