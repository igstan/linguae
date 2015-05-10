structure Subst :> SUBST =
struct
  type ty = (Type.Var.ty * Type.ty) list

  val empty = []

  fun fromList xs = xs

  fun set subst var value =
    (var, value) :: subst

  fun apply subst ty =
    let
      fun fold ((tvar, newTy), ty) = Type.substitute ty tvar newTy
    in
      List.foldl fold ty subst
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
