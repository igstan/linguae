# Damas-Hindley-Milner in Standard ML

### Using SML/NJ:

```
$ sml -Cprint.depth=20
- CM.make "sources.cm";
- open Terms;
opening Terms
  val predef : TypeEnv.ty
  val identity : ty
  val constant : ty
  val compose : ty
  val counter : ty
  val add10 : ty
  val isZero : ty
  val letTerm : ty
  val letPolymorphism : ty
  val closureLetPolymorphism : Term.ty
-
- Infer.typeSignature identity predef;
val it = "forall a. a -> a" : string
-
- Infer.typeSignature constant predef;
val it = "forall ab. a -> b -> a" : string
-
- Infer.typeSignature compose predef;
val it = "forall abc. (c -> b) -> (a -> c) -> a -> b" : string
-
- Infer.typeSignature letPolymorphism predef;
val it = "int" : string
```

### Using MLton:

```bash
$ mlton -output infer sources.mlb
$ ./infer
forall abc. (c -> b) -> (a -> c) -> a -> b
```

### Corner Case

```sml
(* Haskell

  let x1 = \y -> \z -> (z y) y
   in let x2 = \z -> x1 (x1 z)
       in let x3 = \z -> x2 (x2 z)
           in let x4 = \z -> x3 (x3 z) in x4 (\z -> z)
*)

let
  val x1 = fn y => fn z => (z y) y
in
  let
    val x2 = fn z => x1 (x1 z)
  in
    let
      val x3 = fn z => x2 (x2 z)
    in
      let
        val x4 = fn z => x3 (x3 z)
      in
        x4 (fn z => z)
      end
    end
  end
end
```
