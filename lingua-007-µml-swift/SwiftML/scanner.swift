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

typealias Stream = String.CharacterView
typealias Tokens = Array<Token>.SubSequence

let alphaChars = Set("abcdefghijklmnopqrstuvwxyz".characters)
let digitChars = Set("0123456789".characters)
let spaceChars = Set(" \n\r\t".characters)

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
    case .some(let c) where isAlpha(c): return loop(stream.dropFirst(), String(c).characters)
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
    case .some(let c) where isDigit(c): return loop(stream.dropFirst(), String(c).characters)
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
      switch identifier(source) {
        case .some(("when", let rest)): return .Success((.WHEN, rest))
        case .some(("then", let rest)): return .Success((.THEN, rest))
        case .some(("else", let rest)): return .Success((.ELSE, rest))
        case .some(("fn", let rest)): return .Success((.FN, rest))
        case .some(("true", let rest)): return .Success((.TRUE, rest))
        case .some(("false", let rest)): return .Success((.FALSE, rest))
        case .some(("let", let rest)): return .Success((.LET, rest))
        case .some(("in", let rest)): return .Success((.IN, rest))
        case .some(("end", let rest)): return .Success((.END, rest))
        case .some(("val", let rest)): return .Success((.VAL, rest))
        case .some((let id, let rest)): return .Success((.ID(id), rest))
        case .none:
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
