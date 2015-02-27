functor TEnvFn (
  structure TermMap : ORD_MAP where type Key.ord_key = Term.Var.ty
  structure TypeSet : ORD_SET where type Key.ord_key = Type.Var.ty
) :> TENV =
struct
  structure M = TermMap
  structure S = TypeSet

  type ty = TypeScheme.ty M.map

  val empty = M.empty

  fun get tenv var = M.find (tenv, var)

  fun set tenv var ty = M.insert (tenv, var, ty)

  local
    fun insert ((binding, value), env) = set env binding value
  in
    fun fromList bindings = List.foldl insert empty bindings
  end

  fun freeVars tenv =
    List.concat (M.listItems (M.map TypeScheme.freeVars tenv))

  fun generalize tenv ty =
    let
      val tyVars = S.fromList (Type.freeVars ty)
      val envVars = S.fromList (freeVars tenv)
      val schemeVars = S.listItems (S.difference (tyVars, envVars))
    in
      TypeScheme.forall schemeVars ty
    end
end
