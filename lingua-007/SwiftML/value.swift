// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

indirect enum Value<Meta>: CustomStringConvertible {
  case Num(Int)
  case Bool(Bool)
  case Fun(String, Term<Meta>, [String:Value])
  case Primitive((Value) -> Result<Value, String>)

  func success() -> Result<Value, String> {
    return .Success(self)
  }

  var description: String {
    switch self {
      case .Num(let n): return String(n, radix: 10)
      case .Bool(true): return "true"
      case .Bool(false): return "false"
      case .Fun, .Primitive: return "fn"
    }
  }
}
