## Function Composition Signature

If the annotation phase for function application looks like this:

```diff
| Term.APP (def, arg) =>
  let
    val defTy = loop def tenv
    val argTy = loop arg tenv
  in
    TypedTerm.APP (Type.freshVar (), defTy, argTy)
  end
```

Then the type of function composition is: `(c -> b) -> (a -> c) -> a -> b`.

Whereas, when the same phase is like this:

```sml
| Term.APP (def, arg) =>
    TypedTerm.APP (Type.freshVar (), loop def tenv, loop arg tenv)
```

Then the type of function composition is: `(b -> c) -> (a -> b) -> a -> c`.

I think, the latter form is preferable.
