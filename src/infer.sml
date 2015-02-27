structure Infer =
struct
  fun typeOf term =
    let
      open Unify
      val _ = Type.resetFreshness ()
      val typedTerm = annotate term TEnv.empty
      val subst = unify (constrain typedTerm)
      val termTy = TypedTerm.typeOf typedTerm
    in
      Subst.apply subst termTy
    end
end
