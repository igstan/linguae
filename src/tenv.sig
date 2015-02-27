signature TENV =
sig
  type ty

  val empty : ty
  val fromList : (Term.Var.ty * Type.ty) list -> ty
  val get : ty -> Term.Var.ty -> Type.ty option
  val set : ty -> Term.Var.ty -> Type.ty -> ty
end
