signature TYPE_ENV_FN_DEPS =
sig
  structure TermMap : ORD_MAP where type Key.ord_key = Term.Var.ty
  structure TypeSet : ORD_SET where type Key.ord_key = Type.Var.ty
end

functor TypeEnvFn (Deps : TYPE_ENV_FN_DEPS) :> TYPE_ENV =
struct
  structure Map = Deps.TermMap
  structure Set = Deps.TypeSet

  type ty = TypeScheme.ty Map.map

  val empty = Map.empty

  fun get tenv var = Map.find (tenv, var)

  fun set tenv var ty = Map.insert (tenv, var, ty)

  fun fromList bindings =
    let
      fun insert ((binding, value), env) = set env binding value
    in
      List.foldl insert empty bindings
    end

  fun freeVars tenv =
    List.concat (Map.listItems (Map.map TypeScheme.freeVars tenv))

  fun generalize tenv ty =
    let
      val tyVars = Set.fromList (Type.freeVars ty)
      val envVars = Set.fromList (freeVars tenv)
      val schemeVars = Set.listItems (Set.difference (tyVars, envVars))
    in
      TypeScheme.FORALL (schemeVars, ty)
    end

  fun substitute tenv subst =
    let
      fun substTypeScheme typeScheme =
        case typeScheme of
          TypeScheme.SVAR _ => typeScheme
          (* XXX: It might not be ok to substitute bound type variables... *)
        | TypeScheme.FORALL (vars, ty) => TypeScheme.FORALL (vars, Subst.apply subst ty)
    in
      Map.map substTypeScheme tenv
    end

  fun toString tenv =
    let
      open Show
      val pair = tuple2 (fn a => a, TypeScheme.toString)
    in
      list pair (Map.listItemsi tenv)
    end
end
