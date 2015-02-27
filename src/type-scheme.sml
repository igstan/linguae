structure TypeScheme =
struct
  datatype ty = ForAll of Type.Var.ty list * Type.ty

  fun instantiate (ForAll (tyVars, ty)) =
    let
      val fresh = Type.freshVar ()
      fun extend (var, subst) = Subst.set subst var fresh
      val subst = List.foldl extend Subst.empty tyVars
    in
      Subst.apply subst ty
    end

  local
    structure Set = BinarySetFn (Type.Var.Key)
  in
    fun freeVars (ForAll (tyVars, ty)) =
      let
        val tyFreeVars = Set.fromList (Type.freeVars ty)
        val quantFreeVars = Set.fromList tyVars
        val boundVars = Set.difference (tyFreeVars, quantFreeVars)
      in
        Set.listItems boundVars
      end
  end

  fun toString (ForAll (vars, ty)) =
    let
      fun string v = Type.toString (Type.VAR v)
      val forall = String.concat (List.map string vars)
    in
      "forall "^ forall ^". "^ Type.toString ty
    end
end
