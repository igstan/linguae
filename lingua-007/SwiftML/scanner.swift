//
//  SwiftML
//

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

func scan(source: Stream) -> (Token, Stream)? {
  switch source.first {
    case .none: return nil
    case .some("("): return (.LPAREN, source.dropFirst())
    case .some(")"): return (.RPAREN, source.dropFirst())
    case .some("="):
      let source = source.dropFirst()
      switch source.first {
        case .some(">"): return (.DARROW, source.dropFirst())
        case _: return (.EQUAL, source)
      }
    case .some(_):
      switch identifier(source) {
        case .some(("when", let rest)): return (.WHEN, rest)
        case .some(("then", let rest)): return (.THEN, rest)
        case .some(("else", let rest)): return (.ELSE, rest)
        case .some(("fn", let rest)): return (.FN, rest)
        case .some(("true", let rest)): return (.TRUE, rest)
        case .some(("false", let rest)): return (.FALSE, rest)
        case .some(("let", let rest)): return (.LET, rest)
        case .some(("in", let rest)): return (.IN, rest)
        case .some(("end", let rest)): return (.END, rest)
        case .some(("val", let rest)): return (.VAL, rest)
        case .some((let id, let rest)): return (.ID(id), rest)
        case .none:
          switch number(source) {
            case .none: return nil
            case .some(let n, let rest): return (.NUM(n), rest)
          }
      }
  }
}

func scanAll(_ source: Stream) -> [Token] {
  func loop(_ source: Stream, _ acc: [Token]) -> [Token] {
    let trimmed = source.drop(while: isSpace)

    if trimmed.isEmpty {
      return acc
    } else {
      switch scan(source: trimmed) {
        case .none: return acc
        case .some(let token, let rest):
          var acc = acc
          acc.append(token)
          return loop(rest, acc)
      }
    }
  }

  return loop(source, [])
}
