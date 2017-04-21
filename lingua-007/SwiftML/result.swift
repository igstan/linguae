//
//  SwiftML
//

indirect enum Result<S, F> {
  case Success(S)
  case Failure(F)

  public static func fromOptional(_ s: S?, _ f: F) -> Result<S, F> {
    switch s {
      case .some(let s): return .Success(s)
      case .none: return .Failure(f)
    }
  }

  func flatMap<T>(_ fn: (S) -> Result<T, F>) -> Result<T, F> {
    switch self {
      case .Success(let s): return fn(s)
      case .Failure(let f): return .Failure(f)
    }
  }
}
