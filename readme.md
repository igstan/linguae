# Damas-Hindley-Milner in Standard ML

```
$ sml
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
- Infer.typeSignature Terms.identity predef;
val it = "forall a. a -> a" : string
-
- Infer.typeSignature Terms.constant predef;
val it = "forall ab. a -> b -> a" : string
-
- Infer.typeSignature Terms.compose predef;
val it = "forall abc. (c -> b) -> (a -> c) -> a -> b" : string
-
- Infer.typeSignature Terms.letPolymorphism predef;
val it = "int" : string
```
