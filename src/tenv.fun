functor TEnvFn (
  structure TermMap : ORD_MAP where type Key.ord_key = Term.Var.ty
) :> TENV =
struct
  type ty = Type.ty TermMap.map

  val empty = TermMap.empty

  fun get tenv var = TermMap.find (tenv, var)

  fun set tenv var ty = TermMap.insert (tenv, var, ty)

  local
    fun insert ((binding, value), env) = set env binding value
  in
    fun fromList bindings = List.foldl insert empty bindings
  end
end