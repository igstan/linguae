// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

enum TypeError {
  case Circular(Int, Type)
  case Conflict(Type, Type)
  case Unbound(identifier: String)
}
