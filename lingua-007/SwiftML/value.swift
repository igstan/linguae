// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

indirect enum Value<Meta> {
  case Num(Int)
  case Bool(Bool)
  case Fun(String, Term<Meta>, [String:Value])
  case Primitive((Value) -> Result<Value, String>)

  func success() -> Result<Value, String> {
    return .Success(self)
  }

  func toString() -> String {
    switch self {
      case .Num(let n): return String(n)
      case .Bool(true): return "true"
      case .Bool(false): return "false"
      case .Fun(_, _, _), .Primitive(_): return "fn"
    }
  }
}
