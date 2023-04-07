// -------------------------------------------------------------------------- //
// SwiftML                                                                    //
// -------------------------------------------------------------------------- //

func constrain<Attr>(_ term: Term<(Attr, Type)>) -> Set<Constraint> {
  switch term {
    case .Var(_, _): return []
    case let .Num(attr, _): return [.Equal(attr.1, .Int)]
    case let .Bool(attr, _): return [.Equal(attr.1, .Bool)]
    case let .Def(attr, param, body):
      return constrain(body).union([
        .Equal(attr.1, .Fun(param.attr.1, body.attr.1))
      ])
    case let .App(attr, fun, arg):
      return constrain(fun).union(constrain(arg)).union([
        .Equal(fun.attr.1, .Fun(arg.attr.1, attr.1))
      ])
    case let .When(attr, cond, then, otherwise):
      return constrain(cond).union(constrain(then)).union(constrain(otherwise)).union([
        .Equal(cond.attr.1, .Bool),
        .Equal(attr.1, then.attr.1),
        .Equal(attr.1, otherwise.attr.1)
      ])
    case let .Let(attr, binder, value, body):
      return constrain(value).union(constrain(body)).union([
        .Equal(attr.1, body.attr.1),
        .Equal(binder.attr.1, value.attr.1)
      ])
  }
}
