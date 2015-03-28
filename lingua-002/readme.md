# Damas-Hindley-Milner in Standard ML

### Using SML/NJ:

```
$ sml -Cprint.depth=20
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
