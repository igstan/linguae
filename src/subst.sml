structure Subst : SUBST =
struct
  type ty = (Type.Var.ty * Type.ty) list

  val empty = []

  fun fromList xs = xs

  local
    fun substPair ((var, newTy), oldTy) =
      case oldTy of
        Type.VAR var' =>
          if var = var' then newTy else oldTy
      | Type.FUN (param, return) =>
        let
          val paramTy = substPair ((var, newTy), param)
          val returnTy = substPair ((var, newTy), return)
        in
          Type.FUN (paramTy, returnTy)
        end
  in
    fun apply subst ty = List.foldl substPair ty subst
  end

  fun compose s1 s2 = s2 @ s1
end
