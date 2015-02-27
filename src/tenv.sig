signature TENV =
sig
  type ty

  val empty : ty
  val get : ty -> Term.Var.ty -> Type.ty option
  val set : ty -> Term.Var.ty -> Type.ty -> ty
end
