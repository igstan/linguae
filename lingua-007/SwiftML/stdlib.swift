// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

private typealias TypedValue = Value<(Position, Type)>

private func arithmetic(_ a: TypedValue, _ op: @escaping (Int, Int) -> Int) -> Result<TypedValue, String> {
  switch a {
    case let .Num(a): return Value.Primitive({ b in
      switch b {
        case let .Num(b): return Value.Num(op(a, b)).success()
        case _: return .Failure("number required, got: \(b)")
      }
    }).success()
    case _: return .Failure("number required, got: \(a)")
  }
}

let stdlib = [
  "+": Value.Primitive { arithmetic($0, +) },
  "-": Value.Primitive { arithmetic($0, -) },
  "*": Value.Primitive { arithmetic($0, *) },
  "/": Value.Primitive { arithmetic($0, /) }
]
