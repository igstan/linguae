signature TYPE_ENV_FN_DEPS =
sig
  structure TermMap : ORD_MAP where type Key.ord_key = Term.Var.ty
  structure TypeSet : ORD_SET where type Key.ord_key = Type.Var.ty
end

functor TypeEnvFn (Deps : TYPE_ENV_FN_DEPS) :> TYPE_ENV =
struct
  structure M = Deps.TermMap
  structure S = Deps.TypeSet

  type ty = TypeScheme.ty M.map

  val empty = M.empty

  fun get tenv var = M.find (tenv, var)

  fun set tenv var ty = M.insert (tenv, var, ty)

  fun fromList bindings =
    let
      fun insert ((binding, value), env) = set env binding value
    in
      List.foldl insert empty bindings
    end

  fun freeVars tenv =
    List.concat (M.listItems (M.map TypeScheme.freeVars tenv))

  fun generalize tenv ty =
    let
      val tyVars = S.fromList (Type.freeVars ty)
      val envVars = S.fromList (freeVars tenv)
      val schemeVars = S.listItems (S.difference (tyVars, envVars))
    in
      TypeScheme.ForAll (schemeVars, ty)
    end
end
