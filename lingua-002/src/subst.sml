structure Subst :> SUBST =
struct
  type ty = (Type.Var.ty * Type.ty) list

  val empty = []

  fun fromList xs = xs

  fun set subst var value =
    (var, value) :: subst

  local
    fun substPair ((var, newTy), oldTy) =
      case oldTy of
        Type.INT => oldTy
      | Type.BOOL => oldTy
      | Type.VAR var' =>
          if var = var' then newTy else oldTy
      | Type.FUN (param, return) =>
        let
          val paramTy = substPair ((var, newTy), param)
          val returnTy = substPair ((var, newTy), return)
        in
          Type.FUN (paramTy, returnTy)
        end
  in
    fun apply subst ty = List.foldr substPair ty subst
  end

  fun compose s1 s2 =
    let
      val s = List.map (fn (tvar, ty) => (tvar, apply s2 ty)) s1
    in
      s @ s2
    end

  fun toString subst =
    let
      open Show
      val pair = tuple2 (int, Type.toString)
    in
      list pair subst
    end
end
