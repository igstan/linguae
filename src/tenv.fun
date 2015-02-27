functor TEnvFn (
  structure TermMap : ORD_MAP where type Key.ord_key = Term.Var.ty
  structure TypeSet : ORD_SET where type Key.ord_key = Type.Var.ty
) :> TENV =
struct
  structure M = TermMap
  structure S = TypeSet

  type ty = Type.ty M.map

  val empty = M.empty

  fun get tenv var = M.find (tenv, var)

  fun set tenv var ty = M.insert (tenv, var, ty)
end
